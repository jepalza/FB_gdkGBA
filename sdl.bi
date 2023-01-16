#Include "SDL2/SDL.bi"
'#include "SDL2/SDL_mixer.bi"

' modos graficos principales 0,1,2,3 y 4
#define GBA_SCREEN_WIDTH  240
#define GBA_SCREEN_HEIGHT 160

' modo grafico 5 especial 32k colores
#Define GBA_MODE5_WIDTH  160
#Define GBA_MODE5_HEIGHT 128

#define SCREEN_SCALE 2

#define WINDOW_WIDTH  (GBA_SCREEN_WIDTH  * SCREEN_SCALE)
#define WINDOW_HEIGHT (GBA_SCREEN_HEIGHT * SCREEN_SCALE)

#define LINES_VISIBLE  GBA_SCREEN_HEIGHT
#define LINES_TOTAL    228

#define CYC_LINE_TOTAL  1232
#define CYC_LINE_HBLK0  1006
#define CYC_LINE_HBLK1  (CYC_LINE_TOTAL - CYC_LINE_HBLK0)

Dim Shared As UByte Ptr gbascreen 
gbascreen=Callocate ((WINDOW_WIDTH * WINDOW_HEIGHT) -1) 

Dim Shared As uByte x_tiles_lut(15) = { 1, 2, 4, 8, 2, 4, 4, 8, 1, 1, 2, 4, 0, 0, 0, 0 } 
Dim Shared As uByte y_tiles_lut(15) = { 1, 2, 4, 8, 1, 1, 2, 4, 2, 4, 4, 8, 0, 0, 0, 0 } 



static Shared As SDL_Window  Ptr sdlwindow 
Static shared As SDL_Surface Ptr winSurface 
static shared As SDL_Surface Ptr scrSurface 

'Dim Shared As Long tex_pitch 

Declare Sub sdl2_init() 
Declare Sub sdl2_uninit() 


