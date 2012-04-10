(in-package :letcn)

(defun vertex-shader-source ()
"#version 150

in vec4 position;

uniform mat4 model_transform;

void main(void){
  gl_Position = model_transform * position;
}
")

(defun geometry-shader-source ()
"
#version 150

layout(points) in;
layout(triangle_strip, max_vertices = 4) out;

out vec2 uv;

uniform mat4 view_transform;

const float radius = 0.56;

void main(void)
{
  // Behind near clipping plane
  if (gl_in[0].gl_Position.z > -1.0) return;

  vec3 v = normalize(vec3(gl_in[0].gl_Position));
  vec3 right = cross(v, vec3(0.0, 1.0, 0.0));
  vec3 up = cross(right, v);

  up = up * radius;
  right = right * radius;

  v = vec3(gl_in[0].gl_Position);

  vec4 top_right = view_transform * vec4(v + right + up, 1.0);
  vec4 bot_left = view_transform * vec4(v - right - up, 1.0);

  // Out of the viewport
  if (bot_left.x > bot_left.z || bot_left.y > bot_left.z ||
      top_right.x < - top_right.z || top_right.y < - top_right.z)
    return;

  gl_Position = bot_left;
  uv.x = -1.0; uv.y = -1.0;
  EmitVertex();

  gl_Position = view_transform * vec4(v + right - up, 1.0);
  uv.x = 1.0; uv.y = -1.0;
  EmitVertex();

  gl_Position = view_transform * vec4(v - right + up, 1.0);
  uv.x = -1.0; uv.y = 1.0;
  EmitVertex();

  gl_Position = top_right;
  uv.x = 1.0; uv.y = 1.0;
  EmitVertex();

  EndPrimitive();
}
")

(defun fragment-shader-source ()
"#version 150

in vec2 uv;

out vec4 color;

const vec4 ambient_color = vec4(1.0, 0.65, 0.0, 1.0);
const vec4 diffuse_color = vec4(0.2, 0.5, 0.5, 1.0);

uniform vec3 global_light_direction;

void main(){
  if ((uv.x * uv.x + uv.y * uv.y) > 1.0)
    discard;
  else
  {
    vec3 normal = vec3(uv.x, uv.y, sqrt(1 - uv.x * uv.x - uv.y * uv.y));
    float diffuse_brightness = dot(global_light_direction, normal);
    color = ambient_color + diffuse_color * diffuse_brightness;
  }
}
")
