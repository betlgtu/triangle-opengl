#version 330

layout (location = 0) in vec4 position;
layout (location = 1) in vec4 color;

smooth out vec4 theColor;

void main()
{
    gl_Position = position + vec4(0.5, 0.5, 0.0, 1.0);
    theColor = color;
}