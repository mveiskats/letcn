(in-package :letcn)

(defun vertex-shader-source ()
"#version 400

in vec4 position;

uniform mat4 model_transform;

void main(void){
  gl_Position = model_transform * position;
}
")

(defun geometry-shader-source ()
"
#version 400

layout(points) in;
layout(triangle_strip, max_vertices = 4) out;

out vec2 uv;

uniform mat4 view_transform;

const float troct_radius = 0.56;

void main(void)
{
  if (gl_in[0].gl_Position.z > -1.0) return;

  vec3 v = normalize(vec3(gl_in[0].gl_Position));
  vec3 right = cross(v, vec3(0.0, -1.0, 0.0));
  vec3 up = cross(right, v);

  up = up * troct_radius;
  right = right * troct_radius;

  v = vec3(gl_in[0].gl_Position);

  gl_Position = view_transform * vec4(v - right - up, 1.0);
  uv.x = -1.0; uv.y = -1.0;
  EmitVertex();

  gl_Position = view_transform * vec4(v + right - up, 1.0);
  uv.x = 1.0; uv.y = -1.0;
  EmitVertex();

  gl_Position = view_transform * vec4(v - right + up, 1.0);
  uv.x = -1.0; uv.y = 1.0;
  EmitVertex();

  gl_Position = view_transform * vec4(v + right + up, 1.0);
  uv.x = 1.0; uv.y = 1.0;
  EmitVertex();

  EndPrimitive();
}
")

(defun fragment-shader-source ()
"#version 400

in vec3 teh_normal;
in vec2 uv;
out vec4 color;

const vec4 ambient_color = vec4(1.0, 0.65, 0.0, 1.0);
const vec4 diffuse_color = vec4(0.2, 0.5, 0.5, 1.0);

uniform vec3 global_light_direction;

void main(){
  float diffuse_term = clamp(abs(dot(global_light_direction, teh_normal)), 0.0, 1.0);


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
