#ifndef PLATFORM_H
#define PLATFORM_H

#include "Common.h"

typedef enum mouse_flag
{
    MOUSE_MOVE,
    MOUSE_WHEEL,
} mouse_flag;

typedef struct mouse_data 
{
    mouse_flag Event;
    union {
        struct {
            int X, Y;
            bool8 LButton;
            bool8 RButton;
        } Move;
        struct {
            bool8 ScrollTowardUser;
        } Wheel;
    } Status;
} mouse_data;

typedef struct platform_screen_buffer
{
    u32 *Ptr;
    i32 Width, Height;
} platform_screen_buffer;

typedef struct platform_window_dimensions 
{
    i32 Width, Height;
} platform_window_dimensions;



typedef struct background 
{
    double MajorXSpacing;
    double MajorYSpacing;

    u32 MainAxisColor;
    u32 MinorTickColor;
    u32 MajorTickColor;
    u32 BackgroundColor;

    int MinorTickCount;
    int MinorTickThickness;
    int MajorTickThickness;
    int MainAxisThickness;
} background;

typedef struct graph_state 
{
    int PrevMouseX, PrevMouseY;

    double GraphLeft;
    double GraphTop;
    double GraphWidth;
    double GraphHeight;
    double Scale, ScaleInv;

    background Bg;
    bool8 ShouldRedraw;
} graph_state;


/* setters */
void Platform_SetScreenBufferDimensions(int Width, int Height);
void Platform_SetFrameTimeTarget(double MillisecPerFrame);

/* getters */
platform_screen_buffer Platform_GetScreenBuffer(void);
platform_window_dimensions Platform_GetWindowDimensions(void);
double Platform_GetElapsedTimeMs(void); /* starting from right before Graph_OnEntry() */
double Platform_GetFrameTimeMs(void);

/* event request */
void Platform_RequestRedraw(void);

/* main loop */
graph_state Graph_OnEntry(void);
void Graph_OnLoop(graph_state *State);
void Graph_OnExit(graph_state *State);

/* event handlers */
void Graph_OnMouseEvent(graph_state *State, const mouse_data *Mouse);
void Graph_OnRedrawRequest(graph_state *State, platform_screen_buffer *Ctx);

/* misc */
u32 Platform_RGB(u8 r, u8 g, u8 b);


#endif /* PLATFORM_H */

