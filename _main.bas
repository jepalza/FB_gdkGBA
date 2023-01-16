#Include "windows.bi"

Dim Shared As Integer DEB=0

#Undef TRUE
#Ifndef TRUE
	#Define TRUE 1
#EndIf

#Undef FALSE
#Ifndef FALSE
	#Define FALSE 0
#EndIf

'#include "crt\math.bi" ' ceil(), floor(), M_PI, pow(), fabs(), sqrt(), etc
#Include "crt\stdio.bi" ' printf(), scanf(), fopen(), etc
'#Include "crt\stdlib.bi" ' malloc(),calloc(), etc
'#Include "crt\mem.bi" ' memset(var,val,size) -> variable ptr, valor, tamano usar sizeof(variable))
'#Include "crt\string.bi" ' memcpy(dest ptr,orig ptr,long)


#Ifndef BOOL
	#Define BOOL UByte
#EndIf


#include "arm.bi"
#include "arm_mem.bi"
#Include "dma.bi"
#Include "sound.bi"
#include "io.bi"
#include "timer.bi"
#include "sdl.bi"
#include "video.bi"

#include "arm_mem.bas"
#include "arm.bas"
#include "dma.bas"
#include "io.bas"
#include "sdl.bas"
#include "sound.bas"
#include "timer.bas"
#include "video.bas"


    Dim As String gbagame=Command
    Dim As String biosname


    biosname="bios\BIOS_NORMMATT.bin"
    'biosname="bios\gba_bios.bin"

	If gbagame="" Then

	    gbagame="test\hello.gba"
	    'gbagame="test\stripes.gba"
	    'gbagame="test\shades.gba"

   endif 
    
    
    Print "gdkGBA - Gameboy Advance emulator made by gdkchan"
    print "This is FREE software released into the PUBLIC DOMAIN (2018)"
    Print "FreeBasic conversion por Joseba Epalza (2022) <jepalza_gmail_com>"


    arm_init() 
  
    Dim As FILE Ptr biosfile 
    Dim As FILE Ptr cartfile 

    biosfile = fopen(biosname, "rb") 
    if (biosfile = NULL) Then 
        print "Error: GBA BIOS not found!"
        Sleep
        end
    EndIf
    fread(bios, 16384, 1, biosfile) 
    fclose(biosfile) 

    cartfile = fopen(gbagame, "rb") 
    if (cartfile = NULL) Then
        Print "Error: CART file couldn't be opened."
        Print "Make sure that the file exists and the name is correct."
        Sleep
        End
    EndIf
    fseek(cartfile, 0, SEEK_END) 

    cart_rom_size = ftell(cartfile) 
    cart_rom_mask = to_pow2(cart_rom_size) - 1 

    if (cart_rom_size > max_rom_sz) Then cart_rom_size = max_rom_sz 

    fseek(cartfile, 0, SEEK_SET) 
    fread(rom, cart_rom_size, 1, cartfile) 
    fclose(cartfile)

    sdl2_init() 
    arm_reset() 

    Dim As BOOL rungba = TRUE 

    while (rungba)  
        run_frame() 

        Dim As SDL_Event event 

        while (SDL_PollEvent(@event))  
            Select Case (event.type)  
            	Case SDL_KEYDOWN 
                    Select Case (event.key.keysym.sym)  
                    	case SDLK_UP      
                    		key_input.w And=  INV(BTN_U) 
                    	case SDLK_DOWN    
                    		key_input.w And=  INV(BTN_D) 
                    	case SDLK_LEFT    
                    		key_input.w And=  INV(BTN_L) 
                    	case SDLK_RIGHT   
                    		key_input.w And=  INV(BTN_R) 
                    	case SDLK_a       
                    		key_input.w And=  INV(BTN_A) 
                    	case SDLK_s       
                    		key_input.w And=  INV(BTN_B) 
                    	case SDLK_q       
                    		key_input.w And=  INV(BTN_LT) 
                    	case SDLK_w       
                    		key_input.w And=  INV(BTN_RT) 
                    	case SDLK_TAB     
                    		key_input.w And=  INV(BTN_SEL) 
                    	case SDLK_RETURN  
                    		key_input.w And=  INV(BTN_STA) 
                    	case SDLK_d  
                    		DEB=3
                    End Select


            	Case SDL_KEYUP 
                    Select Case (event.key.keysym.sym)  
                    	case SDLK_UP      
                    		key_input.w Or= BTN_U 
                    	case SDLK_DOWN    
                    		key_input.w Or= BTN_D 
                    	case SDLK_LEFT    
                    		key_input.w Or= BTN_L 
                    	case SDLK_RIGHT   
                    		key_input.w Or= BTN_R 
                    	case SDLK_a       
                    		key_input.w Or= BTN_A 
                    	case SDLK_s       
                    		key_input.w Or= BTN_B 
                    	case SDLK_q       
                    		key_input.w Or= BTN_LT 
                    	case SDLK_w       
                    		key_input.w Or= BTN_RT 
                    	case SDLK_TAB     
                    		key_input.w Or= BTN_SEL 
                    	case SDLK_RETURN  
                    		key_input.w Or= BTN_STA 
                    
                   End Select

            	Case SDL_QUIT_  
            		rungba = FALSE 
            
           End Select

        
        Wend
    Wend

    sdl2_uninit() 
    arm_uninit() 
