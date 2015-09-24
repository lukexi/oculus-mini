#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <windows.h> // just for max/min :P
#include "../include/OVR_CAPI_0_6_0.h"
#include "../include/OVR_CAPI_GL.h"
#include "../include/OVR_CAPI_Util.h"
 
/*

[ ] Configure textures in layer
[x] Create framebuffer per texture
[x] Get proper eye projections
[x] Get eye viewports

*/

typedef struct HMDInfo_ {
    ovrHmd             hmd;
    ovrSwapTextureSet *textureSet;
    ovrLayerEyeFov     layer;
    ovrSizei           bufferSize;
    ovrEyeRenderDesc   eyeRenderDesc[2];
    ovrVector3f        hmdToEyeViewOffset[2];
} HMDInfo;



// Creates and configures the HMD for OpenGL rendering
HMDInfo *createHMDInfo(int gl_srgb_alpha8_enum) {
    // Initialize and create the HMD
    ovr_Initialize(0);

    ovrHmd hmd;
    ovrHmd_Create(0, &hmd);

    HMDInfo *hmdInfo = malloc(sizeof(HMDInfo));
    hmdInfo->hmd = hmd;
        
    // Enable all tracking capabilities of the headset
    ovrTrackingCaps trackingCaps = ovrTrackingCap_Orientation  
                                 | ovrTrackingCap_MagYawCorrection
                                 | ovrTrackingCap_Position;
    ovrBool success = ovrHmd_ConfigureTracking(hmd, trackingCaps, 0);
    
    // Find the desired eyes texture size
    ovrSizei recommenedTex0Size = ovrHmd_GetFovTextureSize(hmd, ovrEye_Left,  hmd->DefaultEyeFov[0], 1.0f);
    ovrSizei recommenedTex1Size = ovrHmd_GetFovTextureSize(hmd, ovrEye_Right, hmd->DefaultEyeFov[1], 1.0f);
    ovrSizei bufferSize;
    bufferSize.w = recommenedTex0Size.w + recommenedTex1Size.w;
    bufferSize.h = max(recommenedTex0Size.h, recommenedTex1Size.h);
    hmdInfo->bufferSize = bufferSize;

    // Create the texture set
    ovrSwapTextureSet *textureSet = 0;
    ovrHmd_CreateSwapTextureSetGL(hmd, gl_srgb_alpha8_enum, bufferSize.w, bufferSize.h, &textureSet);
    hmdInfo->textureSet = textureSet;

    // Initialize VR structures, filling out description.
    
    // ovrHmdDesc hmdDesc = ovrHmd_GetHmdDesc(hmd); // 0.7 for DefaultEyeFov
    hmdInfo->eyeRenderDesc[0] = ovrHmd_GetRenderDesc(hmd, ovrEye_Left, hmd->DefaultEyeFov[0]);
    hmdInfo->eyeRenderDesc[1] = ovrHmd_GetRenderDesc(hmd, ovrEye_Right, hmd->DefaultEyeFov[1]);
    hmdInfo->hmdToEyeViewOffset[0] = hmdInfo->eyeRenderDesc[0].HmdToEyeViewOffset;
    hmdInfo->hmdToEyeViewOffset[1] = hmdInfo->eyeRenderDesc[1].HmdToEyeViewOffset;

    // Initialize our single full screen Fov layer.
    hmdInfo->layer.Header.Type      = ovrLayerType_EyeFov;
    hmdInfo->layer.Header.Flags     = ovrLayerFlag_TextureOriginAtBottomLeft;
    hmdInfo->layer.ColorTexture[0]  = textureSet;
    hmdInfo->layer.ColorTexture[1]  = textureSet;
    hmdInfo->layer.Fov[0]           = hmdInfo->eyeRenderDesc[0].Fov;
    hmdInfo->layer.Fov[1]           = hmdInfo->eyeRenderDesc[1].Fov;
    hmdInfo->layer.Viewport[0]      = (ovrRecti){ { 0,                0 } , { bufferSize.w / 2, bufferSize.h } };
    hmdInfo->layer.Viewport[1]      = (ovrRecti){ { bufferSize.w / 2, 0 } , { bufferSize.w / 2, bufferSize.h } };
    // layer.RenderPose is updated later per frame.

    // Reset the pose
    ovrHmd_RecenterPose(hmd);

    return hmdInfo;
}

int getLayerTextureCount(HMDInfo *hmdInfo) {
    return hmdInfo->textureSet->TextureCount;
}

GLuint getLayerTextureIDAtIndex(HMDInfo *hmdInfo, int i) {
    return ((ovrGLTexture)hmdInfo->textureSet->Textures[i]).OGL.TexId;
}

int *getHMDBufferSize(HMDInfo *hmdInfo) {
    return (int *)&hmdInfo->bufferSize;
}

// Call this at the beginning of each frame to find which render target to render to
int getFrameLayerTextureIndex(HMDInfo *hmdInfo) {
    ovrSwapTextureSet *textureSet = hmdInfo->textureSet;
    textureSet->CurrentIndex = (textureSet->CurrentIndex + 1) % textureSet->TextureCount;
    return textureSet->CurrentIndex;
}

int *getLayerViewportForEye(HMDInfo *hmdInfo, int i) {
    int *result = malloc(sizeof(int) * 4);
    result[0] = hmdInfo->layer.Viewport[i].Pos.x;
    result[1] = hmdInfo->layer.Viewport[i].Pos.y;
    result[2] = hmdInfo->layer.Viewport[i].Size.w;
    result[3] = hmdInfo->layer.Viewport[i].Size.h;
    return result;
}

float *newFlatMatrixFromOvrMatrix4f(ovrMatrix4f ovrMatrix) {
    float *matrix = malloc(sizeof(float) * 16);
    for (int i = 0; i < 4; ++i) {
        for (int j = 0; j < 4; ++j) {
            matrix[i*4+j] = ovrMatrix.M[i][j];
        }
    }
    return matrix;
}

float *createProjectionForEye(HMDInfo *info, float znear, float zfar, int i) {
    ovrFovPort *fov = &info->layer.Fov[i];
    ovrMatrix4f projection = ovrMatrix4f_Projection(*fov, znear, zfar, ovrProjection_RightHanded);
    return newFlatMatrixFromOvrMatrix4f(projection);
}

void calcEyePoses(HMDInfo *hmdInfo) {
    // Get both eye poses simultaneously, with IPD offset already included.
    ovrFrameTiming   ftiming  = ovrHmd_GetFrameTiming(hmdInfo->hmd, 0);
    ovrTrackingState hmdState = ovrHmd_GetTrackingState(hmdInfo->hmd, ftiming.DisplayMidpointSeconds);
    ovr_CalcEyePoses(
        hmdState.HeadPose.ThePose, 
        hmdInfo->hmdToEyeViewOffset, 
        hmdInfo->layer.RenderPose);
}

GLuint createMirrorTexture(HMDInfo *hmdInfo, int gl_srgb_alpha8_enum) {
    ovrTexture *outTexture;
    ovrHmd_CreateMirrorTextureGL(
        hmdInfo->hmd, gl_srgb_alpha8_enum, 
        hmdInfo->bufferSize.w, hmdInfo->bufferSize.h, 
        &outTexture);
    return ((ovrGLTexture*)outTexture)->OGL.TexId;
}

void submitFrame(HMDInfo *hmdInfo) {
    ovrHmd hmd = hmdInfo->hmd;
    ovrLayerEyeFov layer = hmdInfo->layer;
    const unsigned int frameIndex = 0; // specifies "next frame after last call"
    const ovrViewScaleDesc* viewScaleDesc = 0; // Specifies default world scale
    unsigned int layerCount = 1;
    const ovrLayerHeader* layers = &layer.Header;
    ovrHmd_SubmitFrame(hmd, frameIndex, viewScaleDesc, &layers, layerCount);
}







float *createOrthoSubProjectionForEye(HMDInfo *hmdInfo, int i, float znear, float zfar) {
    ovrFovPort fov = hmdInfo->eyeRenderDesc[i].Fov;
    float hmdToEyeViewOffsetX = hmdInfo->hmdToEyeViewOffset[i].x;
    ovrVector2f scale = {1.0,1.0};
    float distance = 1;
    ovrMatrix4f projection = ovrMatrix4f_Projection(fov, znear, zfar, ovrProjection_RightHanded);
    ovrMatrix4f orthoProj = ovrMatrix4f_OrthoSubProjection(projection, scale, distance, hmdToEyeViewOffsetX);
    return newFlatMatrixFromOvrMatrix4f(orthoProj);
}



float *poseToArray(const ovrPosef pose) {
    float *orientationAndPosition = malloc(sizeof(float) * 7);
    orientationAndPosition[0] = pose.Orientation.x;
    orientationAndPosition[1] = pose.Orientation.y;
    orientationAndPosition[2] = pose.Orientation.z;
    orientationAndPosition[3] = pose.Orientation.w;
    orientationAndPosition[4] = pose.Position.x;
    orientationAndPosition[5] = pose.Position.y;
    orientationAndPosition[6] = pose.Position.z;
    return orientationAndPosition;
}

float *getOrientationAndPositionForEye(HMDInfo *hmdInfo, int i) {
    return poseToArray(hmdInfo->layer.RenderPose[i]);
}

const float* getHMDPose(HMDInfo *hmdInfo) {
    ovrTrackingState trackingState = ovrHmd_GetTrackingState(hmdInfo->hmd, 0.0);
    ovrPosef headPose = trackingState.HeadPose.ThePose;
    return poseToArray(headPose);
}

void recenterPose(HMDInfo *hmdInfo) {
    ovrHmd_RecenterPose(hmdInfo->hmd);
}

void setPerformanceHUD(HMDInfo *hmdInfo, int i) {
    ovrPerfHudMode perfHudMode = ovrPerfHud_Off;
    if (i == 1) {
        perfHudMode = ovrPerfHud_LatencyTiming;
    } else if  (i == 2) {
        perfHudMode = ovrPerfHud_RenderTiming;
    }
    ovrHmd_SetInt(hmdInfo->hmd, "PerfHudMode", (int)perfHudMode);
}   