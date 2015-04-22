#version 330 core
in vec3 f_color;
out vec4 color;

void main(void) {
  color = vec4(f_color.x, f_color.y, f_color.z, 1.0);
}