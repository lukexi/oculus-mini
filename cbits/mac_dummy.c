
typedef void HMDInfo;
typedef int GLuint;
typedef void* ovrMatrix4f;


HMDInfo *createHMDInfo(int gl_srgb_alpha8_enum) {
  return 0;
}

int getLayerTextureCount(HMDInfo *hmdInfo) {
  return 0;
}


GLuint getLayerTextureIDAtIndex(HMDInfo *hmdInfo, int i) {
  return 0;
}


int *getHMDBufferSize(HMDInfo *hmdInfo) {
  return 0;
}

int getFrameLayerTextureIndex(HMDInfo *hmdInfo) {
  return 0;
}

int *getLayerViewportForEye(HMDInfo *hmdInfo, int i) {
  return 0;
}

float *newFlatMatrixFromOvrMatrix4f(ovrMatrix4f ovrMatrix) {
  return 0;
}

float *createProjectionForEye(HMDInfo *info, float znear, float zfar, int i) {
  return 0;
}


void calcEyePoses(HMDInfo *hmdInfo) {
  return;
}

GLuint createMirrorTexture(HMDInfo *hmdInfo, int gl_srgb_alpha8_enum) {
  return 0;
}

void submitFrame(HMDInfo *hmdInfo) {
  return;
}

float *createOrthoSubProjectionForEye(HMDInfo *hmdInfo, int i, float znear, float zfar) {
  return 0;
}

float *getOrientationAndPositionForEye(HMDInfo *hmdInfo, int i) {
  return 0;
}


const float* getHMDPose(HMDInfo *hmdInfo) {
  return 0;
}

void recenterPose(HMDInfo *hmdInfo) {
  return;
}
