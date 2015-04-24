#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../include/OVR_CAPI_0_5_0.h"
#include "../include/OVR_CAPI_GL.h"
#include "../include/OVR_CAPI_Util.h"

ovrHmd createHMD() {
    ovr_Initialize(0);

    ovrHmd hmd = ovrHmd_Create(0);
    if (hmd == 0) {
        hmd = ovrHmd_CreateDebug(ovrHmd_DK2);
    }

    return hmd;
}

// Configures the HMD and returns the ovrEyeRenderDescs for each eye
const ovrEyeRenderDesc *configureHMD(ovrHmd hmd) {

    // Enable all tracking capabilities of the headset
    ovrTrackingCaps trackingCaps = ovrTrackingCap_Orientation  
                                 | ovrTrackingCap_MagYawCorrection
                                 | ovrTrackingCap_Position;
    ovrBool success = ovrHmd_ConfigureTracking(hmd, trackingCaps, 0);
    
    // Configure the Oculus rendering to use OpenGL, at the HMD's resolution
    union ovrGLConfig cfg;
    memset(&cfg, 0, sizeof(cfg));
    cfg.OGL.Header.API = ovrRenderAPI_OpenGL;
    cfg.OGL.Header.BackBufferSize = hmd->Resolution;
    cfg.OGL.Header.Multisample = 0;
    
    // Configure features of the Oculus distortion rendering
    ovrDistortionCaps distortionCaps = ovrDistortionCap_TimeWarp
                                     | ovrDistortionCap_Vignette
                                     | ovrDistortionCap_Overdrive 
                                     | ovrDistortionCap_HqDistortion;
    
    ovrEyeRenderDesc *eyeRenderDescs = malloc(sizeof(ovrEyeRenderDesc) * 2);
    int configResult = ovrHmd_ConfigureRendering(hmd, &cfg.Config, distortionCaps, hmd->MaxEyeFov, eyeRenderDescs);
    
    // Configure the HMD display
    ovrHmdCaps hmdCaps = ovrHmdCap_ExtendDesktop 
                       | ovrHmdCap_LowPersistence
                       | ovrHmdCap_DynamicPrediction;
    ovrHmd_SetEnabledCaps(hmd, hmdCaps);

    // Reset the pose
    ovrHmd_RecenterPose(hmd);

    return eyeRenderDescs;
}

const ovrVector3f *getEyeRenderDesc_HmdToEyeViewOffsets(const ovrEyeRenderDesc eyeRenderDescs[2]) {
    // Grab the eyeViewOffsets needed for configure the camera and passing to ovr_EndFrame
    ovrVector3f *eyeViewOffsets = malloc(sizeof(ovrVector3f) * 2);
    eyeViewOffsets[0] = eyeRenderDescs[0].HmdToEyeViewOffset;
    eyeViewOffsets[1] = eyeRenderDescs[1].HmdToEyeViewOffset;
    return eyeViewOffsets;
}

const ovrFovPort *getEyeRenderDesc_FOV(const ovrEyeRenderDesc eyeRenderDescs[2], int eyeIndex) {

    // Grab the eyeViewOffsets needed for configure the camera and passing to ovr_EndFrame
    ovrFovPort *eyeFovPort = malloc(sizeof(ovrFovPort));
    *eyeFovPort = eyeRenderDescs[eyeIndex].Fov;
    return eyeFovPort;
}

const int *getHMDResolution(ovrHmd hmd) {
    int *hmdResolution = malloc(sizeof(int) * 2);
    hmdResolution[0] = hmd->Resolution.w;
    hmdResolution[1] = hmd->Resolution.h;
    return hmdResolution;
}

const int *getHMDRenderTargetSize(ovrHmd hmd) {
    // Find out how large the OVR recommends each eye texture should be
    ovrSizei recommenedTex0Size = ovrHmd_GetFovTextureSize(hmd, ovrEye_Left, hmd->DefaultEyeFov[ovrEye_Left], 1.0);
    ovrSizei recommenedTex1Size = ovrHmd_GetFovTextureSize(hmd, ovrEye_Right, hmd->DefaultEyeFov[ovrEye_Right], 1.0);

    // Combine their sizes so we can use one large texture for both eyes
    int renderTargetSizeW = recommenedTex0Size.w + recommenedTex1Size.w;
    int renderTargetSizeH = recommenedTex0Size.h > recommenedTex1Size.h ? recommenedTex0Size.h : recommenedTex1Size.h;

    int *renderTargetSize = malloc(sizeof(int) * 2);
    renderTargetSize[0] = renderTargetSizeW;
    renderTargetSize[1] = renderTargetSizeH;
    return renderTargetSize;
}



float *getEyeProjection(ovrFovPort *fov, float znear, float zfar) {
    ovrMatrix4f projection = ovrMatrix4f_Projection(*fov, znear, zfar, ovrProjection_RightHanded);
    float *matrix = malloc(sizeof(float) * 16);
    for (int i = 0; i < 4; ++i) {
        for (int j = 0; j < 4; ++j) {
            matrix[i*4+j] = projection.M[i][j];
        }
    }
    return matrix;
}

float *getPoses_OrientationAndPositionForEye(const ovrPosef *eyePoses, int eyeIndex) {
    float *orientationAndPosition = malloc(sizeof(float) * 7);
    orientationAndPosition[0] = eyePoses[eyeIndex].Orientation.x;
    orientationAndPosition[1] = eyePoses[eyeIndex].Orientation.y;
    orientationAndPosition[2] = eyePoses[eyeIndex].Orientation.z;
    orientationAndPosition[3] = eyePoses[eyeIndex].Orientation.w;
    orientationAndPosition[4] = eyePoses[eyeIndex].Position.x;
    orientationAndPosition[5] = eyePoses[eyeIndex].Position.y;
    orientationAndPosition[6] = eyePoses[eyeIndex].Position.z;
    return orientationAndPosition;
}

// Bundle up the eye texture and its measurements into configuration structs to pass to OVR API
const ovrTexture *createOVRTextureArray(GLuint eyeTexture, int width, int height) {

    ovrSizei textureSize = (ovrSizei){width, height};

    ovrSizei viewportSize = (ovrSizei){width / 2, height};

    // Configure the viewport for rendering to the left eye
    ovrRecti leftViewport;
    leftViewport.Pos = (ovrVector2i){0, 0};
    leftViewport.Size = viewportSize;

    // Create the texture header for the left eye
    ovrTextureHeader textureHeader0;
    textureHeader0.API = ovrRenderAPI_OpenGL;
    textureHeader0.TextureSize = textureSize;
    textureHeader0.RenderViewport = leftViewport;

    // Configure the viewport for rendering to the right eye
    ovrRecti rightViewport;
    rightViewport.Pos = (ovrVector2i){width / 2, 0};
    rightViewport.Size = viewportSize;

    // Create the texture header for the right eye
    ovrTextureHeader textureHeader1;
    textureHeader1.API = ovrRenderAPI_OpenGL;
    textureHeader1.TextureSize = textureSize;
    textureHeader1.RenderViewport = rightViewport;

    // We use one large combined texture for both eyes, so we configure accordingly.
    ovrGLTexture eyeGLTexture0;
    eyeGLTexture0.OGL.Header = textureHeader0;
    eyeGLTexture0.OGL.TexId  = eyeTexture;

    ovrGLTexture eyeGLTexture1;
    eyeGLTexture1.OGL.Header = textureHeader1;
    eyeGLTexture1.OGL.TexId  = eyeTexture;

    ovrTexture *eyeTextureData = malloc(sizeof(ovrTexture) * 2);
    eyeTextureData[0] = eyeGLTexture0.Texture;
    eyeTextureData[1] = eyeGLTexture1.Texture;

    return eyeTextureData;
}

void beginFrame(ovrHmd hmd) {
    ovrHmd_BeginFrame(hmd, 0);
}

const ovrPosef *getEyePoses(ovrHmd hmd, const ovrVector3f hmdToEyeViewOffset[2]) {
    ovrPosef *eyePoses = malloc(sizeof(ovrPosef) * 2);
    ovrHmd_GetEyePoses(hmd, 0, hmdToEyeViewOffset, eyePoses, 0);
    return eyePoses;
}
