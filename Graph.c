#include "Platform.h"
#include <stdio.h>
#include <math.h>
#include <stdlib.h>

#define SWAP(typ, a, b) do {\
    typ t = a;\
    a = b;\
    b = t;\
} while (0)
#define IN_RANGE(lower, n, upper) ((lower) <= (n) && (n) <= (upper))
#define GRAPH_X(p_ctx, x) (x)
#define GRAPH_Y(p_ctx, y) (y)
#define IDX(p_ctx, x, y) (GRAPH_Y(p_ctx, y)*(p_ctx)->Width + GRAPH_X(p_ctx, x))
#define POKE(p_ctx, x, y) \
    if (IN_RANGE(0, x, (p_ctx)->Width - 1) \
    && IN_RANGE(0, y, (p_ctx)->Height - 1))\
        (p_ctx)->Ptr[IDX(p_ctx, x, y)]
#define PEEK(p_ctx, x, y) \
    ((IN_RANGE(0, x, (p_ctx)->Width - 1) \
    && IN_RANGE(0, y, (p_ctx)->Height - 1)) \
    ? (p_ctx)->Ptr[IDX(p_ctx, x, y)] : 0)

#define GRAPH_TO_SCR_X(x) GraphToScrX(x, State->ScaleInv, State->GraphLeft)
#define GRAPH_TO_SCR_Y(y) GraphToScrY(y, State->ScaleInv, State->GraphTop)
#define GRAPH_FROM_SCR_X(x) GraphFromScrX(x, State->Scale, State->GraphLeft)
#define GRAPH_FROM_SCR_Y(y) GraphFromScrX(y, State->Scale, State->GraphTop)



static double Fn(double x)
{
    return x*x*x - x - 3*x*x;
}

static double GraphFromScrY(int y, double Scale, double GraphTop)
{
    return -(y * Scale - GraphTop);
}

static double GraphFromScrX(int x, double Scale, double GraphLeft)
{
    return x * Scale + GraphLeft;
}

static int GraphToScrY(double y, double ScaleInv, double GraphTop)
{
    return (-y + GraphTop) * ScaleInv;
}

static int GraphToScrX(double x, double ScaleInv, double GraphLeft)
{
    return (x - GraphLeft) * ScaleInv;
}

static double FloorToMultiple(double x, double Multiple)
{
    return floor(x/Multiple) * Multiple;
}



static void DrawLine(platform_screen_buffer *Ctx, int x0, int y0, int x1, int y1, int Thickness, u32 Color)
{
    int Dx = abs(x1 - x0);
    int Dy = abs(y1 - y0);
    if (Dx > Dy) /* the line will be more horizontal */
    {
        if (x0 > x1)
        {
            SWAP(int, x0, x1);
            SWAP(int, y0, y1);
        }
        int AccumY = 0;
        int y = y0;
        int Inc = y0 < y1? 1 : -1;
        for (int x = x0; x < x1; x++)
        {
            for (int i = 0; i < Thickness; i++)
            {
                POKE(Ctx, x, y + i) = Color;
            }
            AccumY += Dy;
            if (AccumY >= Dx)
            {
                AccumY -= Dx;
                y += Inc;
            }
        }
    }
    else
    {
        if (y0 > y1)
        {
            SWAP(int, x0, x1);
            SWAP(int, y0, y1);
        }
        int AccumX = 0;
        int x = x0;
        int Inc = x0 < x1? 1 : -1;
        for (int y = y0; y < y1; y++)
        {
            for (int i = 0; i < Thickness; i++)
            {
                POKE(Ctx, x + i, y) = Color;
            }
            AccumX += Dx;
            if (AccumX >= Dy)
            {
                AccumX -= Dy;
                x += Inc;
            }
        }
    }
}


static void GraphUpdateScaling(graph_state *State, const platform_screen_buffer *Ctx)
{
    State->Scale = State->GraphWidth / Ctx->Width;
    State->ScaleInv = 1.0 / State->Scale;
}

static void GraphDrawLine(const graph_state *State, platform_screen_buffer *Ctx, double x0, double y0, double x1, double y1, int Thickness, u32 Color)
{
    int ScrX0 = GRAPH_TO_SCR_X(x0);
    int ScrY0 = GRAPH_TO_SCR_Y(y0);
    int ScrX1 = GRAPH_TO_SCR_X(x1);
    int ScrY1 = GRAPH_TO_SCR_Y(y1);
    DrawLine(Ctx, ScrX0, ScrY0, ScrX1, ScrY1, Thickness, Color); 
}



graph_state Graph_OnEntry(void)
{
    int ScrWidth = 720, ScrHeight = 720;
    Platform_SetScreenBufferDimensions(ScrWidth, ScrHeight);
    Platform_SetFrameTimeTarget(1000.0 / 500);

    graph_state State = {
        .GraphTop = 4.5,
        .GraphLeft = -4.5,
        .GraphWidth = 9.0,
        .GraphHeight = 9.0,
    };
    State.Scale = State.GraphWidth / ScrWidth;
    State.ScaleInv = 1 / State.Scale;
    return State;
}

void Graph_OnLoop(graph_state *State)
{
    double MsPerFrame = Platform_GetFrameTimeMs();
    double FPS = 1000 / MsPerFrame;
    printf("\rt: %2.2f, l: %2.2f, w: %2.2f, h: %2.2f, t=%.3fs, f=%3.2ffps - %4.2fms", 
        State->GraphTop, 
        State->GraphLeft, 
        State->GraphWidth, 
        State->GraphHeight,
        Platform_GetElapsedTimeMs() * .001,
        FPS, MsPerFrame
    );
}

void Graph_OnExit(graph_state *State)
{
}


void Graph_OnMouseEvent(graph_state *State, const mouse_data *Mouse)
{
    platform_window_dimensions Win = Platform_GetWindowDimensions();
    bool8 ShouldRedraw = false;
    switch (Mouse->Event)
    {
    case MOUSE_MOVE:
    {
        int MouseX = Mouse->Status.Move.X;
        int MouseY = Mouse->Status.Move.Y;
        if (Mouse->Status.Move.LButton)
        {
            double Dx = GraphFromScrX(MouseX - State->PrevMouseX, State->GraphWidth / Win.Width, 0);
            double Dy = GraphFromScrY(MouseY - State->PrevMouseY, State->GraphHeight / Win.Height, 0);
            /* mouse direction is opposite that of the top left point of the graph */
            State->GraphLeft -= Dx; 
            State->GraphTop -= Dy;
            ShouldRedraw = true;
        }
        /* always update mouse pos */
        State->PrevMouseX = MouseX;
        State->PrevMouseY = MouseY;
    } break;
    case MOUSE_WHEEL:
    {
        double ZoomScale = Mouse->Status.Wheel.ScrollTowardUser
            ? 1.05
            : 1/1.05;

        double MouseX = GraphFromScrX(State->PrevMouseX, (double)State->GraphWidth / Win.Width, State->GraphLeft);
        double MouseY = GraphFromScrY(State->PrevMouseY, (double)State->GraphHeight / Win.Height, State->GraphTop);

        double ScaledLeft = (State->GraphLeft - MouseX) * ZoomScale + MouseX;
        double ScaledTop = (State->GraphTop - MouseY) * ZoomScale + MouseY;
        double ScaledWidth = State->GraphWidth * ZoomScale;
        double ScaledHeight = State->GraphHeight * ZoomScale;

        State->GraphTop = ScaledTop;
        State->GraphWidth = ScaledWidth;
        State->GraphHeight = ScaledHeight;
        State->GraphLeft = ScaledLeft;
        ShouldRedraw = true;
    } break;
    }

    if (ShouldRedraw)
    {
        Platform_RequestRedraw();
    }
}

void Graph_OnRedrawRequest(graph_state *State, platform_screen_buffer *Ctx)
{   
    GraphUpdateScaling(State, Ctx);
    u32 Black = 0;
    u32 Bg = Platform_RGB(0xD0, 0xD0, 0xD0);

    double MarkWidthX = State->GraphHeight / 50.0 * .5;
    double MarkWidthY = MarkWidthX;
    int TickCountX = 10;
    int TickCountY = 10;
    int XAxisThickness = 2;
    int YAxisThickness = 2;
    int GraphThickness = 3;
    int TickThickness = 1;

    int Width = Ctx->Width;
    int Height = Ctx->Height;
    {
        u32 *Ptr = Ctx->Ptr;
        for (int y = 0; y < Height; y++)
        {
            for (int x = 0; x < Width; x++)
            {
                *Ptr++ = Bg;
            }
        }
    }


    /* graph the fn */
    int PrevX = INT_MIN;
    int PrevY;
    for (int x = 0; x < Width; x++)
    {
        double X = GRAPH_FROM_SCR_X(x);
        int y = GRAPH_TO_SCR_Y(Fn(X));
        if (PrevX != INT_MIN)
        {
            DrawLine(Ctx, PrevX, PrevY, x, y, GraphThickness, Black);
        }
        PrevX = x;
        PrevY = y;
    }

    /* y axis */
    double YAxisTickDelta = (State->GraphHeight / TickCountY);
    GraphDrawLine(State, Ctx, 0, State->GraphTop, 0, State->GraphTop - State->GraphHeight, YAxisThickness, Black);
    for (double i = 0; i < State->GraphHeight; i += YAxisTickDelta)
    {
        double Y = FloorToMultiple(i - (State->GraphHeight - State->GraphTop), YAxisTickDelta);
        GraphDrawLine(State, Ctx, -MarkWidthY, Y, MarkWidthY, Y, TickThickness, Black);
    }

    /* x axis */
    double XAxisTickDelta = (State->GraphWidth / TickCountX);
    GraphDrawLine(State, Ctx, State->GraphLeft, 0, State->GraphLeft + State->GraphWidth, 0, XAxisThickness, Black);
    for (double i = 0; i < State->GraphWidth; i += XAxisTickDelta)
    {
        double X = FloorToMultiple(i + State->GraphLeft, XAxisTickDelta);
        GraphDrawLine(State, Ctx, X, -MarkWidthX, X, MarkWidthX, TickThickness, Black);
    }
}





