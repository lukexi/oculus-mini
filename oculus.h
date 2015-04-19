#include "include/OVR_CAPI_0_5_0.h"
#include "include/OVR_CAPI_GL.h"

ovrHmd createHMD();

const ovrVector3f *configureHMD(ovrHmd hmd);

const int *getHMDRenderTargetSize(ovrHmd hmd);

const ovrTexture *createOVRTextureArray(GLuint eyeTexture, int width, int height);

void beginFrame(ovrHmd hmd);

const ovrPosef *getEyePoses(ovrHmd hmd, const ovrVector3f hmdToEyeViewOffset[2]);