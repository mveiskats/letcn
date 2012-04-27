(in-package :letcn)

(defun vertex-shader-source ()
"#version 150

in vec3 position;
in vec3 foo;

out PerVertex {
  vec4 position;
  vec3 color;
} vert;

uniform mat4 model_transform;

void main(void){
  vert.position = model_transform * vec4(position, 1.0);
  vert.color = foo;
}
")

(defun geometry-shader-source ()
"
#version 150

layout(points) in;
layout(triangle_strip, max_vertices = 4) out;

in PerVertex {
  vec4 position;
  vec3 color;
} vert[];

out vec3 sprite_color;
out vec2 uv;

uniform mat4 view_transform;

const float radius = 0.56;

void main(void)
{
  // Behind near clipping plane
  if (vert[0].position.z > -1.0) return;

  vec3 v = normalize(vec3(vert[0].position));
  vec3 right = cross(v, vec3(0.0, 1.0, 0.0));
  vec3 up = cross(right, v);

  up = up * radius;
  right = right * radius;

  v = vec3(vert[0].position);

  vec4 top_right = view_transform * vec4(v + right + up, 1.0);
  vec4 bot_left = view_transform * vec4(v - right - up, 1.0);

  // Viewport culling - still a bit wonky
  // if (bot_left.x > bot_left.z || bot_left.y > bot_left.z ||
  //    top_right.x < - top_right.z || top_right.y < - top_right.z)
  //  return;

  sprite_color = vert[0].color;

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

in vec3 sprite_color;
in vec2 uv;

out vec3 fragment_color;

const vec3 ambient_light = vec3(0.2, 0.2, 0.2);
const vec3 diffuse_light = vec3(0.9, 0.9, 0.9);

uniform vec3 global_light_direction;

void main(){
  if ((uv.x * uv.x + uv.y * uv.y) > 1.0)
    discard;
  else
  {
    vec3 normal = vec3(uv.x, uv.y, sqrt(1 - uv.x * uv.x - uv.y * uv.y));
    float diffuse_brightness = dot(global_light_direction, normal);
    vec3 total_light = ambient_light + diffuse_light * diffuse_brightness;

    fragment_color = sprite_color * total_light;
  }
}
")
