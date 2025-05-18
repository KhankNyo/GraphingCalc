#include "Include/Platform.h"
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

#define MAX(a, b) ((a) > (b)? (a) : (b))


typedef double graph_to_scr_fn(double x, double ScaleInv, double Left);


static double Fn(double x)
{
    return x*x;
}



static double Frac(double x)
{
    return x - (i64)x;
}

static double RecipFrac(double x)
{
    return 1 - Frac(x);
}

static double Round(double x)
{
    return (i64)(x + .5);
}


static u32 RGB(u8 r, u8 g, u8 b)
{
    return (u32)r << 16 | (u32)g << 8 | (u32)b;
}

static u32 Grayscale(u8 Grayness)
{
    return RGB(Grayness, Grayness, Grayness);
}

/* divide A and B into 8 parts and mix them according to alpha */
static u32 RGBMix(u32 A, u32 B, double AlphaA)
{
    u32 A8 = (A / 8) & 0x001F1F1F;
    u32 B8 = (B / 8) & 0x001F1F1F;
    switch ((int)Round(AlphaA*8))
    {
    case 0: return B;
    case 1: return A8 + (B - B8);
    case 2: return 2*A8 + (B - 2*B8);
    case 3: return 3*A8 + (B - 3*B8);
    case 4: return (A/2 & 0x007F7F7F) + (B/2 & 0x007F7F7F);
    case 5: return 3*B8 + (A - 3*A8);
    case 6: return 2*B8 + (A - 2*A8);
    case 7: return B8 + (A - A8);
    default:
    case 8: return A;
    }
}


/* NOTE: Ctx must already initialized with a background color */
static void GraphMixColorWithBg(platform_screen_buffer *Ctx, int x, int y, u32 Color, double Alpha)
{
    if (!IN_RANGE(0, x, Ctx->Width - 1) 
    || !IN_RANGE(0, y, Ctx->Height - 1))
    {
        return;
    }

    int Index = IDX(Ctx, x, y);
    u32 Bg = Ctx->Ptr[Index];
    Ctx->Ptr[Index] = RGBMix(Color, Bg, Alpha);
}





static double GraphFromScrY(int y, double Scale, double GraphTop)
{
    return -(y * Scale - GraphTop);
}

static double GraphFromScrX(int x, double Scale, double GraphLeft)
{
    return x * Scale + GraphLeft;
}

static double GraphToScrY(double y, double ScaleInv, double GraphTop)
{
    return (-y + GraphTop) * ScaleInv;
}

static double GraphToScrX(double x, double ScaleInv, double GraphLeft)
{
    return (x - GraphLeft) * ScaleInv;
}

static double FloorToMultiple(double x, double Multiple)
{
    return floor(x/Multiple) * Multiple;
}




static void DrawLineBresenham(platform_screen_buffer *Ctx, int x0, int y0, int x1, int y1, int Thickness, u32 Color)
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
        for (int x = x0; x <= x1; x++)
        {
            for (int i = -Thickness/2; i < Thickness/2 + Thickness % 2; i++)
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
        for (int y = y0; y <= y1; y++)
        {
            for (int i = -Thickness/2; i < Thickness/2 + Thickness % 2; i++)
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

static void DrawLineXiaolinWu(platform_screen_buffer *Ctx, double x0, double y0, double x1, double y1, int Thickness, u32 Color)
{
    double Dy = fabs(y1 - y0);
    double Dx = fabs(x1 - x0);
    bool8 MoreVertical = Dy > Dx;

    if (MoreVertical)
    {
        SWAP(double, x0, y0);
        SWAP(double, x1, y1);
    }
    if (x0 > x1)
    {
        SWAP(double, x0, x1);
        SWAP(double, y0, y1);
    }

    Dy = fabs(y1 - y0);
    Dx = fabs(x1 - x0);
    double Gradient = Dx == 0
        ? 1.0 
        : (double)Dy / Dx;

    /* first endpoint */
    int X0 = Round(x0);
    double Y = y0 + Gradient * (X0 - x0);

    /* second endpoint */
    int X1 = Round(x1);

    /* main loop */
    if (MoreVertical)
    {
        for (int x = X0; x <= X1; x++)
        {
            int y = Y;
            for (int i = -Thickness/2; i < Thickness/2 + Thickness % 2; i++)
            {
                GraphMixColorWithBg(Ctx, y, x + i, Color, RecipFrac(Y));
                GraphMixColorWithBg(Ctx, y + 1, x + i, Color, Frac(Y));
            }
            Y += Gradient;
        }
    }
    else
    {
        for (int x = X0; x <= X1; x++)
        {
            int y = Y;
            for (int i = -Thickness/2; i < Thickness/2 + Thickness % 2; i++)
            {
                GraphMixColorWithBg(Ctx, x + i, y, Color, RecipFrac(Y));
                GraphMixColorWithBg(Ctx, x + i, y + 1, Color, Frac(Y));
            }
            Y += Gradient;
        }
    }
}




static void GraphUpdateScaling(graph_state *State, const platform_screen_buffer *Ctx)
{
    State->Scale = State->GraphWidth / Ctx->Width;
    State->ScaleInv = 1.0 / State->Scale;
}

static void GraphDrawLine(
    const graph_state *State, platform_screen_buffer *Ctx, double x0, double y0, double x1, double y1, int Thickness, u32 Color)
{
    double ScrX0 = GRAPH_TO_SCR_X(x0);
    double ScrY0 = GRAPH_TO_SCR_Y(y0);
    double ScrX1 = GRAPH_TO_SCR_X(x1);
    double ScrY1 = GRAPH_TO_SCR_Y(y1);
    DrawLineXiaolinWu(Ctx, ScrX0, ScrY0, ScrX1, ScrY1, Thickness, Color); 
}


static void GraphUpdateAxis(const graph_state *State, int CtxWidth, axis *A, graph_to_scr_fn GraphToScr)
{
    A->TickLength = State->GraphHeight / 50.0 * .5;
    if ((int)GraphToScr(A->MajorTickSpacing, State->ScaleInv, 0) > CtxWidth/10)
    {
        A->MajorTickSpacing /= 2;
    }
    else if ((int)GraphToScr(A->MajorTickSpacing, State->ScaleInv, 0) < CtxWidth/20)
    {
        A->MajorTickSpacing *= 2;
    }
}



graph_state Graph_OnEntry(void)
{
    int ScrWidth = 1080, ScrHeight = 720;
    double AspectRatio = (double)ScrHeight / ScrWidth;
    Platform_SetScreenBufferDimensions(ScrWidth, ScrHeight);
    Platform_SetFrameTimeTarget(1000.0 / 144);

    graph_state State = {
        .GraphLeft = -10,
        .GraphWidth = 20,
        .GraphTop = 10 * AspectRatio,
        .GraphHeight = 20 * AspectRatio,

        .X.TickLength = ScrHeight/50.0 * .5,
        .X.MajorTickSpacing = 2.0,

        .Y.TickLength = ScrHeight/50.0 * .5,
        .Y.MajorTickSpacing = 2.0,
    };
    State.Scale = State.GraphWidth / ScrWidth;
    State.ScaleInv = 1 / State.Scale;
    return State;
}

void Graph_OnLoop(graph_state *State)
{
    double MsPerFrame = Platform_GetFrameTimeMs();
    double FPS = 1000 / MsPerFrame;
#if 1
    printf("\rt: %2.2f, l: %2.2f, w: %2.2f, h: %2.2f, t=%.3fs, f=%3.2ffps - %4.2fms", 
        State->GraphTop, 
        State->GraphLeft, 
        State->GraphWidth, 
        State->GraphHeight,
        Platform_GetElapsedTimeMs() * .001,
        FPS, MsPerFrame
    );
#endif

    if (State->ShouldRedraw)
    {
        Platform_RequestRedraw();
        State->ShouldRedraw = false;
    }
}

void Graph_OnExit(graph_state *State)
{
}


void Graph_OnMouseEvent(graph_state *State, const mouse_data *Mouse)
{
    platform_window_dimensions Win = Platform_GetWindowDimensions();
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
            State->ShouldRedraw = true;
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
        State->ShouldRedraw = true;
    } break;
    }
}

void Graph_OnRedrawRequest(graph_state *State, platform_screen_buffer *Ctx)
{   
    GraphUpdateScaling(State, Ctx);

    double GraphBottom = State->GraphTop - State->GraphHeight;
    double GraphRight = State->GraphLeft + State->GraphWidth;
    u32 GraphColor = RGB(0xFF, 0x80, 0x00);
    u32 MainAxisColor = RGB(0, 0, 0);
    u32 MinorTickColor = Grayscale(0xC0);
    u32 MajorTickColor = Grayscale(0x90);
    u32 Bg = Grayscale(0xD0);
    int Width = Ctx->Width;
    int Height = Ctx->Height;
    int MinorTickCount = 3;

    int GraphThickness = 3;
    int MajorTickThickness = 3;
    int MinorTickThickness = 1;
    int XAxisThickness = 3;
    int YAxisThickness = 3;


    GraphUpdateAxis(State, Width, &State->X, GraphToScrX);
    GraphUpdateAxis(State, Width, &State->Y, GraphToScrX);


    /* background */
    u32 *Ptr = Ctx->Ptr;
    for (int y = 0; y < Height; y++)
    {
        for (int x = 0; x < Width; x++)
        {
            *Ptr++ = Bg;
        }
    }

    /* x axis */
    int SpaceCount = MinorTickCount + 1;
    double MinorTickSpacing = State->X.MajorTickSpacing / SpaceCount;
    for (double i = 0; i < State->GraphWidth + State->X.MajorTickSpacing; i += State->X.MajorTickSpacing)
    {
        double X = FloorToMultiple(i + State->GraphLeft, State->X.MajorTickSpacing);
        /* major tick */
        if (X != 0.0)
        {
            /* TODO: tick vs axis */
            //GraphDrawLine(State, Ctx, X, -State->X.TickLength, X, State->X.TickLength, MajorTickThickness, MajorTickColor);
            GraphDrawLine(State, Ctx, X, State->GraphTop, X, GraphBottom, MajorTickThickness, MajorTickColor);
        }
        for (int k = 1; k <= MinorTickCount; k++)
        {
            GraphDrawLine(State, Ctx, 
                X + k*MinorTickSpacing, State->GraphTop, 
                X + k*MinorTickSpacing, GraphBottom, 
                MinorTickThickness, MinorTickColor
            );
        }
    }
    GraphDrawLine(State, Ctx, State->GraphLeft, 0, GraphRight, 0, XAxisThickness, MainAxisColor);


    /* y axis */
    for (double i = 0; i < State->GraphHeight + State->Y.MajorTickSpacing; i += State->Y.MajorTickSpacing)
    {
        double Y = FloorToMultiple(i - (State->GraphHeight - State->GraphTop), State->Y.MajorTickSpacing);
        /* major tick */
        if (Y != 0.0)
        {
            //GraphDrawLine(State, Ctx, -State->Y.TickLength, Y, State->Y.TickLength, Y, MajorTickThickness, MajorTickColor);
            GraphDrawLine(State, Ctx, State->GraphLeft, Y, GraphRight, Y, MajorTickThickness, MajorTickColor);
        }
        /* minor tick */
        for (int k = 1; k <= MinorTickCount; k++)
        {
            GraphDrawLine(State, Ctx, 
                State->GraphLeft, Y + k*MinorTickSpacing,
                GraphRight, Y + k*MinorTickSpacing, 
                MinorTickThickness, MinorTickColor
            );
        }
    }
    GraphDrawLine(State, Ctx, 0, State->GraphTop, 0, GraphBottom, YAxisThickness, MainAxisColor);


    /* function graph */
    double PrevX = State->GraphLeft;
    double PrevY = Fn(PrevX);
    for (int x = 1; x < Width; x++)
    {
        double X = GRAPH_FROM_SCR_X(x);
        double Y = Fn(X);
        if (IN_RANGE(GraphBottom, PrevY, State->GraphTop) 
        || IN_RANGE(GraphBottom, Y, State->GraphTop))
        {
            GraphDrawLine(State, Ctx, PrevX, PrevY, X, Y, GraphThickness, GraphColor);
        }
        PrevX = X;
        PrevY = Y;
    }
}





