
Sub sdl2_init() 
    SDL_Init(SDL_INIT_VIDEO Or SDL_INIT_AUDIO) 


    sdlwindow  = SDL_CreateWindow("gbaemu", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, WINDOW_WIDTH, WINDOW_HEIGHT, SDL_WINDOW_SHOWN) 
    if sdlwindow=0 Then 
        Print "SDL_CreateWindow() failed: ", SDL_GetError() 
        Sleep:end 
    End If
    
    winSurface = SDL_GetWindowSurface(sdlwindow) 
    if winSurface=0 Then 
        Print"SDL_GetWindowSurface() failed: ", SDL_GetError() 
        Sleep:end 
    End If
    
    scrSurface = SDL_CreateRGBSurface(0, GBA_SCREEN_WIDTH, GBA_SCREEN_HEIGHT, 32, &h000000ff, &h0000ff00, &h00ff0000, &hff000000) 
    if scrSurface=0 Then 
        print"SDL_CreateRGBSurface() failed: ", SDL_GetError() 
        Sleep:end 
    End If




	'freq as long
	'format as SDL_AudioFormat
	'channels as Uint8
	'silence as Uint8
	'samples as Uint16
	'padding as Uint16
	'size as Uint32
	'callback as SDL_AudioCallback
	'userdata as any Ptr
    Dim As SDL_AudioSpec spec
    With spec
        .freq     = SND_FREQUENCY  ' 32KHz
        .format   = AUDIO_S16SYS   ' Signed 16 bits System endiannes
        .channels = SND_CHANNELS   ' Stereo
        '.silence
        .samples  = SND_SAMPLES    ' 16ms
        '.padding
        '.size
        .callback = @sound_mix     ' rutina -> sound_mix(data_ As Integer Ptr ,  stream As uByte Ptr , len_ As Long) 
        .userdata = NULL
    End With 



    SDL_OpenAudio(@spec, NULL) 
    SDL_PauseAudio(0) 
End Sub

Sub sdl2_uninit() 
    SDL_FreeSurface(scrSurface) 
    SDL_DestroyWindow(sdlwindow) 

    SDL_CloseAudio() 
    SDL_Quit() 
End Sub
