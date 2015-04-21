#include "../include/OVR_CAPI_0_5_0.h"
#include "../include/OVR_CAPI_GL.h"
#include "../include/OVR_CAPI_Util.h"

ovrHmd createHMD();

const ovrEyeRenderDesc *configureHMD(ovrHmd hmd);

const ovrVector3f *getEyeRenderDesc_HmdToEyeViewOffsets(const ovrEyeRenderDesc eyeRenderDescs[2]);
const ovrFovPort *getEyeRenderDesc_FOV(const ovrEyeRenderDesc eyeRenderDescs[2], int eyeIndex);

const int *getHMDRenderTargetSize(ovrHmd hmd);

const ovrTexture *createOVRTextureArray(GLuint eyeTexture, int width, int height);

void beginFrame(ovrHmd hmd);

const ovrPosef *getEyePoses(ovrHmd hmd, const ovrVector3f hmdToEyeViewOffset[2]);