(in-package :letcn)

(defun vertex-shader-source ()
"#version 400

in vec4 position;
uniform mat4 mvp;

void main(void){
  gl_Position = mvp * position;
}
")

(defun geometry-shader-source ()
"#version 400

layout(triangles) in;
layout(line_strip, max_vertices = 3) out;

uniform mat4 mvp;

in Data{
  vec3 normal;
  vec4 position;
} vdata[3];

out Data{
  vec3 color;
} gdata;

void main(){
  for(int i = 0; i < gl_in.length(); i++) {
    gl_Position = gl_in[i].gl_Position;
    EmitVertex();
  }
  EndPrimitive();

  vec4 middle = (vdata[0].position + vdata[1].position + vdata[2].position)/3;
  middle.w = 1.0;
  vec3 normal = normalize((vdata[0].normal + vdata[1].normal + vdata[2].normal)/3);

  gl_Position = mvp * middle;
  gdata.color = vec3(0);
  EmitVertex();

  gl_Position = mvp * (middle + vec4(normal*0.4, 0));
  gdata.color = normal*0.5+0.5;
  EmitVertex();

  EndPrimitive();
}
")

(defun fragment-shader-source ()
"#version 400

out vec3 color;

void main(){
  color = vec3(0.2, 0.7, 0.2);
}
")
