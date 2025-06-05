
#include <string.h> /* memset */
#include <stdio.h>
#include <x86intrin.h>

#include "Common.h"
#include "Platform.h"
#include "Jit.h"

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

#define GRAPH_TO_SCR_X(x) Graph_ToScrX(x, State->ScaleInv, State->GraphLeft)
#define GRAPH_TO_SCR_Y(y) Graph_ToScrY(y, State->ScaleInv, State->GraphTop)
#define GRAPH_FROM_SCR_X(x) Graph_FromScrX(x, State->Scale, State->GraphLeft)
#define GRAPH_FROM_SCR_Y(y) Graph_FromScrX(y, State->Scale, State->GraphTop)





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
static void Graph_MixColorWithBg(platform_screen_buffer *Ctx, int x, int y, u32 Color, double Alpha)
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





static double Graph_FromScrY(int y, double Scale, double GraphTop)
{
    return -(y * Scale - GraphTop);
}

static double Graph_FromScrX(int x, double Scale, double GraphLeft)
{
    return x * Scale + GraphLeft;
}

static double Graph_ToScrY(double y, double ScaleInv, double GraphTop)
{
    return (-y + GraphTop) * ScaleInv;
}

static double Graph_ToScrX(double x, double ScaleInv, double GraphLeft)
{
    return (x - GraphLeft) * ScaleInv;
}

static double FloorToMultiple(double x, double Multiple)
{
    return (i64)(x/Multiple) * Multiple;
}




static void DrawLineXiaolinWu(platform_screen_buffer *Ctx, double x0, double y0, double x1, double y1, int Thickness, u32 Color)
{
    double Dy = AbsF(y1 - y0);
    double Dx = AbsF(x1 - x0);
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

    Dy = (y1 - y0);
    Dx = (x1 - x0);
    double Gradient = Dx == 0
        ? 1.0 
        : (double)Dy / Dx;

    /* first endpoint */
    int X0 = Round(x0);
    double Y = y0 + Gradient * (X0 - x0);

    /* second endpoint */
    int X1 = Round(x1);

    /* main loop */
    int VerticalIndex = 0;
    if (MoreVertical)
    {
        for (int x = X0; x <= X1; x++)
        {
            int y = Y;
            for (int i = -Thickness/2; i < Thickness/2 + Thickness%2; i++)
            {
                Graph_MixColorWithBg(Ctx, y + VerticalIndex, x + i, Color, RecipFrac(Y));
                Graph_MixColorWithBg(Ctx, y + 1 + VerticalIndex, x + i, Color, Frac(Y));
            }
            Y += Gradient;
        }
    }
    else
    {
        for (int x = X0; x <= X1; x++)
        {
            int y = Y;
            for (int i = -Thickness/2; i < Thickness/2 + Thickness%2; i++)
            {
                Graph_MixColorWithBg(Ctx, x + i, y + VerticalIndex, Color, RecipFrac(Y));
                Graph_MixColorWithBg(Ctx, x + i, y + 1 + VerticalIndex, Color, Frac(Y));
            }
            Y += Gradient;
        }
    }
}

static void Graph_DrawLine(
    const graph_state *State, platform_screen_buffer *Ctx, double x0, double y0, double x1, double y1, int Thickness, u32 Color)
{    
    double ScrX0 = GRAPH_TO_SCR_X(x0);
    double ScrY0 = GRAPH_TO_SCR_Y(y0);
    double ScrX1 = GRAPH_TO_SCR_X(x1);
    double ScrY1 = GRAPH_TO_SCR_Y(y1);
    DrawLineXiaolinWu(Ctx, ScrX0, ScrY0, ScrX1, ScrY1, Thickness, Color); 
}



static void Graph_UpdateScaling(graph_state *State, int CtxWidth)
{
    State->Scale = State->GraphWidth / CtxWidth;
    State->ScaleInv = 1.0 / State->Scale;
}

static void Graph_UpdateBackground(graph_state *State, int CtxWidth)
{
    int ZoomOutThreshold = CtxWidth/16;
    int ZoomInThreshold = CtxWidth/8;
    if ((int)Graph_ToScrX(State->Bg.MajorXSpacing, State->ScaleInv, 0) > ZoomInThreshold)
    {
        State->Bg.MajorXSpacing /= 2;
    }
    else if ((int)Graph_ToScrX(State->Bg.MajorXSpacing, State->ScaleInv, 0) < ZoomOutThreshold)
    {
        State->Bg.MajorXSpacing *= 2;
    }

    if ((int)Graph_ToScrX(State->Bg.MajorYSpacing, State->ScaleInv, 0) > ZoomInThreshold)
    {
        State->Bg.MajorYSpacing /= 2;
    }
    else if ((int)Graph_ToScrX(State->Bg.MajorYSpacing, State->ScaleInv, 0) < ZoomOutThreshold)
    {
        State->Bg.MajorYSpacing *= 2;
    }
}

static void Graph_DrawBackground(const graph_state *State, platform_screen_buffer *Ctx)
{
    double GraphLeft = State->GraphLeft;
    double GraphTop = State->GraphTop;
    double GraphBottom = State->GraphTop - State->GraphHeight;
    double GraphRight = State->GraphLeft + State->GraphWidth;
    int Width = Ctx->Width;
    int Height = Ctx->Height;

    u32 MainAxisColor = State->Bg.MainAxisColor;
    u32 MinorTickColor = State->Bg.MinorTickColor;
    u32 MajorTickColor = State->Bg.MajorTickColor;
    u32 Bg = State->Bg.BackgroundColor;

    int MinorTickCount = State->Bg.MinorTickCount;
    int MajorTickThickness = State->Bg.MajorTickThickness;
    int MinorTickThickness = State->Bg.MinorTickThickness;
    int MainAxisThickness = State->Bg.MainAxisThickness;

    /* background */
#if 0
    u32 *Ptr = Ctx->Ptr;
    for (int y = 0; y < Height; y++)
    {
        for (int x = 0; x < Width; x++)
        {
            *Ptr++ = Bg;
        }
    }
#elif 0
    uint VecSize = 4;
    u32 Size = Height*Width;
    u32 VecCount = Size / VecSize;
    u32 Remain = Size % VecSize;
    u32 *Ptr = Ctx->Ptr;
    __m128i BackgroundColor = _mm_set1_epi32(Bg);
    for (uint i = 0; i < VecCount; i++)
    {
        _mm_storeu_si128(Ptr, BackgroundColor);
        Ptr += VecSize;
    }
    for (uint i = 0; i < Remain; i++)
    {
        *Ptr++ = Bg;
    }
#else
    uint Count = Height*Width;
    u32 *Ptr = Ctx->Ptr;
    /* store til cache line */
    while ((uintptr_t)Ptr % 32 && Count > 0)
    {
        *Ptr++ = Bg;
        Count--;
    }
    uint VecSize = 8;
    uint VecCount = Count / VecSize;
    uint Remain = Count % VecSize;
    __m256i BackgroundColor = _mm256_set1_epi32(Bg);
    /* store til remainder */
    for (uint i = 0; i < VecCount; i++)
    {
        _mm256_store_si256(Ptr, BackgroundColor);
        Ptr += VecSize;
    }
    /* store til end */
    for (uint i = 0; i < Remain; i++)
    {
        *Ptr++ = Bg;
    }

#endif
    
    /* minor ticks */
    /* x */
    int SpaceCount = MinorTickCount + 1;
    double MinorTickSpacing = State->Bg.MajorXSpacing / SpaceCount;
    for (double x = GraphLeft - State->Bg.MajorXSpacing; 
        x < GraphRight + State->Bg.MajorXSpacing; 
        x += State->Bg.MajorXSpacing)
    {
        double MajorX = FloorToMultiple(x, State->Bg.MajorXSpacing);
        for (int k = 1; k <= MinorTickCount; k++)
        {
            double MinorX = MajorX + k*MinorTickSpacing;
            if (MinorX <= GraphLeft || GraphRight <= MinorX)
                continue;

            Graph_DrawLine(
                State, Ctx, 
                MinorX, GraphTop, 
                MinorX, GraphBottom,
                MinorTickThickness,
                MinorTickColor
            );
        }
    }
    /* y */
    for (double y = GraphBottom - State->Bg.MajorXSpacing; 
        y < GraphTop + State->Bg.MajorYSpacing; 
        y += State->Bg.MajorYSpacing)
    {
        double MajorY = FloorToMultiple(y, State->Bg.MajorYSpacing);
        for (int k = 1; k <= MinorTickCount; k++)
        {
            double MinorY = MajorY + k*MinorTickSpacing;
            if (MinorY <= GraphBottom || GraphTop <= MinorY)
                continue;

            Graph_DrawLine(
                State, Ctx, 
                GraphLeft, MinorY, 
                GraphRight, MinorY,
                MinorTickThickness,
                MinorTickColor
            );
        }
    }

    /* major ticks/axes */
    /* x */
    for (double x = GraphLeft; 
        x < GraphRight + State->Bg.MajorXSpacing; 
        x += State->Bg.MajorXSpacing)
    {
        double MajorX = FloorToMultiple(x, State->Bg.MajorXSpacing);
        if (0 == MajorX || !IN_RANGEX(GraphLeft, MajorX, GraphRight))
            continue;
        Graph_DrawLine(
            State, Ctx, 
            MajorX, GraphTop, 
            MajorX, GraphBottom,
            MajorTickThickness, 
            MajorTickColor
        );
    }
    /* y */
    for (double y = GraphBottom; 
        y < GraphTop + State->Bg.MajorYSpacing; 
        y += State->Bg.MajorYSpacing)
    {
        double MajorY = FloorToMultiple(y, State->Bg.MajorYSpacing);

        if (0 == MajorY || !IN_RANGEX(GraphBottom, MajorY, GraphTop))
            continue;
        Graph_DrawLine(
            State, Ctx, 
            GraphLeft, MajorY, 
            GraphRight, MajorY,
            MajorTickThickness, 
            MajorTickColor
        );
    }
    /* Main axes */
    /* x */
    Graph_DrawLine(
        State, Ctx, 
        0, GraphTop, 
        0, GraphBottom,
        MainAxisThickness, 
        MainAxisColor
    );
    /* y */
    Graph_DrawLine(
        State, Ctx, 
        GraphLeft, 0, 
        GraphRight, 0, 
        MainAxisThickness, 
        MainAxisColor
    );
}


static jit_result sResult;
graph_state Graph_OnEntry(void)
{
    int ScrWidth = 1080, ScrHeight = 720;
    double AspectRatio = (double)ScrHeight / ScrWidth;
    Platform_SetScreenBufferDimensions(ScrWidth, ScrHeight);
    Platform_SetFrameTimeTarget(0);

    uint ScratchpadCapacity = 128 * 1024;
    uint ProgramMemCapacity = 32*1024;
    uint GlobalMemCapacity = 2*1024;
    uint DefTableCapacity = 4*1024;
    void *Scratchpad = Platform_AllocateMemory(ScratchpadCapacity);
    void *ProgramMemory = Platform_AllocateMemory(ProgramMemCapacity);
    double *GlobalMemory = Platform_AllocateMemory(GlobalMemCapacity * sizeof(double));
    def_table_entry *DefTableArray = Platform_AllocateMemory(DefTableCapacity * sizeof(def_table_entry));

    graph_state State = {
        .GraphLeft = -10,
        .GraphWidth = 20,
        .GraphTop = 10 * AspectRatio,
        .GraphHeight = 20 * AspectRatio,

        .Bg = {
            .MajorXSpacing = 2.0,
            .MajorYSpacing = 2.0,

            .MainAxisColor = Grayscale(0),
            .MinorTickColor = Grayscale(0xC0),
            .MajorTickColor = Grayscale(0x90),
            .BackgroundColor = Grayscale(0xD0),

            .MinorTickCount = 3,

            .MajorTickThickness = 2,
            .MinorTickThickness = 1,
            .MainAxisThickness = 3,
        },

    };
    Graph_UpdateScaling(&State, ScrWidth);
    assert( 0 == Jit_Init(
            &State.Jit, 
            Scratchpad, ScratchpadCapacity,
            GlobalMemory, GlobalMemCapacity, 
            ProgramMemory, ProgramMemCapacity, 
            DefTableArray, DefTableCapacity
        )
    );


    const char *Expr = 
        "f(x) = x*x\n"
        ;
    sResult = Jit_Compile(&State.Jit, Expr);
    if (sResult.ErrMsg)
    {
        printf("%s\n", sResult.ErrMsg);
    }
    else
    {
        printf("Compilation OK, calling init: \n");
        Platform_EnableExecution(ProgramMemory, ProgramMemCapacity);
        jit_init Init = Jit_GetInit(&State.Jit, &sResult);
        Init(GlobalMemory);
        printf("Call OK.\n");
    }

    return State;
}

void Graph_OnLoop(graph_state *State)
{
    static double ElapsedPrev;
    static double AvgFPS, AvgMsPerFrame;
    static double FrameCount;
    double ElapsedNow = Platform_GetElapsedTimeMs();
    if (ElapsedNow - ElapsedPrev > 500)
    {
        AvgMsPerFrame = (ElapsedNow - ElapsedPrev) / FrameCount;
        AvgFPS = FrameCount / ((ElapsedNow - ElapsedPrev) / 1000);
        FrameCount = 0;
        ElapsedPrev = ElapsedNow;
    }
    FrameCount++;

#if 1
    printf("\rt: %g, l: %g, w: %g, h: %g, t=%.3fs, f=%3.2ffps - %4.2fms      ", 
        State->GraphTop, 
        State->GraphLeft, 
        State->GraphWidth, 
        State->GraphHeight,
        Platform_GetElapsedTimeMs() * .001,
        AvgFPS, AvgMsPerFrame
    );
#endif

    State->GraphInvalidated = true;
    if (State->GraphInvalidated)
    {
        platform_screen_buffer Screen = Platform_GetScreenBuffer();
        State->GraphInvalidated = false;


        /* compute x and y values of each function */
        assert(Screen.Width < (int)STATIC_ARRAY_SIZE(State->OutputArray[0]));
        def_table_entry *i = sResult.GlobalSymbol;
        State->GraphCount = 0;
        memset(State->ArrayCount, 0, sizeof(State->ArrayCount));
        while (i && State->GraphCount < STATIC_ARRAY_SIZE(State->OutputArray))
        {
            /* TODO: GetFnPtr returns init routine, which it shouldnt do */
            typedef double (*graphable_fn)(double *GlobalData, double Param);
            graphable_fn Fn = (graphable_fn)Jit_GetFunctionPtr(&State->Jit, &i->As.Function);
            for (int x = 0; x < Screen.Width; x++)
            {
                double GraphX = GRAPH_FROM_SCR_X(x);
                double GraphY = Fn(sResult.GlobalData, GraphX);

                if (IN_RANGE(State->GraphTop - State->GraphHeight, GraphY, State->GraphTop))
                {
                    int Index = State->ArrayCount[State->GraphCount]++;
                    State->OutputArray[State->GraphCount][Index] = GRAPH_TO_SCR_Y(GraphY);
                    State->InputArray[State->GraphCount][Index] = GRAPH_TO_SCR_X(GraphX);
                }

            }
            i = i->Next;
            State->GraphCount++;
        }
    }
    Platform_RequestRedraw();
}

void Graph_OnExit(graph_state *State)
{
    (void)State;
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
            double Dx = Graph_FromScrX(MouseX - State->PrevMouseX, State->GraphWidth / Win.Width, 0);
            double Dy = Graph_FromScrY(MouseY - State->PrevMouseY, State->GraphHeight / Win.Height, 0);
            /* mouse direction is opposite that of the top left point of the graph */
            State->GraphLeft -= Dx; 
            State->GraphTop -= Dy;
            State->GraphInvalidated = true;
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

        double MouseX = Graph_FromScrX(State->PrevMouseX, (double)State->GraphWidth / Win.Width, State->GraphLeft);
        double MouseY = Graph_FromScrY(State->PrevMouseY, (double)State->GraphHeight / Win.Height, State->GraphTop);

        double ScaledLeft = (State->GraphLeft - MouseX) * ZoomScale + MouseX;
        double ScaledTop = (State->GraphTop - MouseY) * ZoomScale + MouseY;
        double ScaledWidth = State->GraphWidth * ZoomScale;
        double ScaledHeight = State->GraphHeight * ZoomScale;

        State->GraphTop = ScaledTop;
        State->GraphWidth = ScaledWidth;
        State->GraphHeight = ScaledHeight;
        State->GraphLeft = ScaledLeft;
        State->GraphInvalidated = true;
    } break;
    }
}

void Graph_OnRedrawRequest(graph_state *State, platform_screen_buffer *Ctx)
{   
    int Width = Ctx->Width;
    Graph_UpdateScaling(State, Width);
    Graph_UpdateBackground(State, Width);

    double GraphLeft = State->GraphLeft;
    double GraphTop = State->GraphTop;
    double GraphBottom = State->GraphTop - State->GraphHeight;

    u32 GraphColor = RGB(0xFF, 0x80, 0x00);
    int GraphThickness = 3;

    Graph_DrawBackground(State, Ctx);


    if (sResult.ErrMsg)
        return;

    u32 GraphColors[4] = {
        RGB(0xFF, 0x80, 0xFF),
        RGB(0, 0xFF, 0x80),
        RGB(0, 0x80, 0xFF),
        GraphColor
    };

    /* graph each function */
#if 0
    def_table_entry *i = sResult.GlobalSymbol;
    while (i)
    {
        if (i->Type == TYPE_FUNCTION && i->As.Function.ParamCount == 1)
        {
            typedef double (*graphable_fn)(double *GlobalData, double Param);
            graphable_fn Fn = (graphable_fn)Jit_GetFunctionPtr(&State->Jit, &i->As.Function);

            double PrevX = GraphLeft;
            double PrevY = Fn(sResult.GlobalData, PrevX);
            for (int x = 1; x < Width; x++)
            {
                double X = GRAPH_FROM_SCR_X(x);
                double Y = Fn(sResult.GlobalData, X);
                if (IN_RANGE(GraphBottom, PrevY, GraphTop) 
                || IN_RANGE(GraphBottom, Y, GraphTop))
                {
                    Graph_DrawLine(State, Ctx, PrevX, PrevY, X, Y, GraphThickness, Colors[k]);
                }
                PrevX = X;
                PrevY = Y;
            }
            k = (k + 1) % STATIC_ARRAY_SIZE(Colors);
        }
        i = i->Next;
    }
#else
    for (uint k = 0; k < State->GraphCount; k++)
    {
        uint ColorSelect = k % STATIC_ARRAY_SIZE(GraphColors);
        for (uint i = 1; i < State->ArrayCount[k]; i++)
        {
            double PrevX = State->InputArray[k][i - 1];
            double PrevY = State->OutputArray[k][i - 1];
            double X = State->InputArray[k][i];
            double Y = State->OutputArray[k][i];
            DrawLineXiaolinWu(Ctx, PrevX, PrevY, X, Y, GraphThickness, GraphColors[ColorSelect]);
        }
    }
#endif
}



