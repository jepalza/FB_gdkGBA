


Sub RGB_to_mem(addr As ULong, dat As ULong, alfa As Integer=0)
	gbascreen[addr+3]=(dat And &h000000ff) ' alfa
	If alfa=1 Then gbascreen[addr+3]=10 ' transparencia 
	gbascreen[addr+0]=(dat And &h0000ff00) Shr 8  'r
	gbascreen[addr+1]=(dat And &h00ff0000) Shr 16 'g
	gbascreen[addr+2]=(dat And &hff000000) Shr 24 'b
End Sub


Sub render_obj(prio As uByte) 
    if (disp_cnt.w And OBJ_ENB)=0 Then return 

    Dim As UByte obj_index 
    Dim As ULong offset = &h3f8 

    Dim As ULong surf_addr = v_count.w * GBA_SCREEN_WIDTH * 4 

    for obj_index = 0 To 127         
        Dim As UShort attr0 = oam[offset + 0] Or (oam[offset + 1] Shl 8) 
        Dim As UShort attr1 = oam[offset + 2] Or (oam[offset + 3] Shl 8) 
        Dim As UShort attr2 = oam[offset + 4] Or (oam[offset + 5] Shl 8) 

        offset -= 8 

        Dim As Short  obj_y    = (attr0 Shr  0) And &hff 
        Dim As BOOL   affine   = (attr0 Shr  8) And &h1 
        Dim As BOOL   dbl_size = (attr0 Shr  9) And &h1 
        Dim As BOOL   hidden   = (attr0 Shr  9) And &h1 
        Dim As UByte  obj_shp  = (attr0 Shr 14) And &h3 
        Dim As UByte  affine_p = (attr1 Shr  9) And &h1f 
        Dim As UByte  obj_size = (attr1 Shr 14) And &h3 
        Dim As UByte  chr_prio = (attr2 Shr 10) And &h3 

        If (chr_prio <> prio) Or ((affine=0) And (hidden<>0)) Then continue For

        Dim As Short pa, pb, pc, pd 
        pa = &h100  '1.0
        pd = &h100  '1.0
        pb = &h000  '0.0
        pc = &h000  '0.0

        if (affine) Then 
            Dim As ULong p_base = affine_p * 32 

            pa = oam[p_base + &h06] Or (oam[p_base + &h07] Shl 8) 
            pb = oam[p_base + &h0e] Or (oam[p_base + &h0f] Shl 8) 
            pc = oam[p_base + &h16] Or (oam[p_base + &h17] Shl 8) 
            pd = oam[p_base + &h1e] Or (oam[p_base + &h1f] Shl 8) 
        EndIf
  

        Dim As UByte lut_idx = obj_size Or (obj_shp Shl 2) 

        Dim As UByte x_tiles = x_tiles_lut(lut_idx) 
        Dim As UByte y_tiles = y_tiles_lut(lut_idx) 

        Dim As Long rcx = x_tiles * 4 
        Dim As Long rcy = y_tiles * 4 

        if (affine<>0) And (dbl_size<>0) Then 
            rcx *= 2 
            rcy *= 2 
        EndIf

        if (obj_y + rcy * 2) > &hff Then obj_y -= &h100 

        if (obj_y <= CLng(v_count.w)) And ((obj_y + rcy * 2) > v_count.w) Then 
  
            Dim As UByte  obj_mode = (attr0 Shr 10) And &h3 
            Dim As BOOL   mosaic   = (attr0 Shr 12) And &h1 
            Dim As BOOL   is_256   = (attr0 Shr 13) And &h1 
            Dim As Short  obj_x    = (attr1 Shr  0) And &h1ff 
            Dim As BOOL   flip_x   = (attr1 Shr 12) And &h1 
            Dim As BOOL   flip_y   = (attr1 Shr 13) And &h1 
            Dim As UShort chr_numb = (attr2 Shr  0) And &h3ff 
            Dim As UByte  chr_pal  = (attr2 Shr 12) And &hf 

            Dim As ULong chr_base = &h10000 Or chr_numb * 32 

            obj_x Shl = 7 
            obj_x Shr = 7 

            Dim As Long x, y = v_count.w - obj_y 

            if (affine=0) And (flip_y<>0) Then y Xor= (y_tiles * 8) - 1 

            Dim As UByte tsz = IIf(is_256 , 64 , 32)  'Tile block size (in bytes, = (8 * 8 * bpp) / 8)
            Dim As UByte lsz = IIf(is_256 ,  8 ,  4)  'Pixel line row size (in bytes)

            Dim As Long ox = pa * -rcx + pb * (y - rcy) + (x_tiles Shl 10) 
            Dim As Long oy = pc * -rcx + pd * (y - rcy) + (y_tiles Shl 10) 

            if (affine=0) And (flip_x<>0) Then 
                ox = (x_tiles * 8 - 1) Shl 8 
                pa = -&h100 
            EndIf

            Dim As ULong tys = IIf(disp_cnt.w And MAP_1D_FLAG , x_tiles * tsz , 1024)  'Tile row stride

            Dim As ULong ADDRESS = surf_addr + obj_x * 4 

            for  x = 0 To (rcx * 2)-1         
                if (obj_x + x) < 0 Then GoTo continueFor1
                if (obj_x + x) >= GBA_SCREEN_WIDTH Then Exit For 

                Dim As ULong vram_addr 
                Dim As ULong pal_idx 

                Dim As UShort tile_x = ox Shr 11 
                Dim As UShort tile_y = oy Shr 11 

                If (ox < 0) Or (tile_x >= x_tiles) Then GoTo ContinueFor1 
                If (oy < 0) Or (tile_y >= y_tiles) Then GoTo continueFor1

                Dim As UShort chr_x = (ox Shr 8) And 7 
                Dim As UShort chr_y = (oy Shr 8) And 7 

                Dim As ULong chr_addr = _
                    chr_base       + _
                    tile_y   * tys + _
                    chr_y    * lsz 

                if (is_256) Then 
                    vram_addr = chr_addr + tile_x * 64 + chr_x 
                    pal_idx   = vram[vram_addr] 
                Else
                    vram_addr = chr_addr + tile_x * 32 + (chr_x Shr 1) 
                    pal_idx   = (vram[vram_addr] Shr (chr_x And 1) * 4) And &hf 
                EndIf

                Dim As ULong pal_addr = &h100 Or pal_idx Or iif( is_256=0 , chr_pal * 16 , 0) 

                If (pal_idx) Then RGB_to_mem(ADDRESS , palette_(pal_addr) )
                
                ' esto no sirve, algunos juegos se muestran transparentes
                'If (pal_idx) Then RGB_to_mem(ADDRESS , palette_(pal_addr),IIf(pal_idx>15,1,0) )
                
            continuefor1:
                ox+=pa
                oy+=pc
                ADDRESS+=4
            Next
        EndIf

    Next

End Sub

Dim Shared As uByte bg_enb(2) = { &hf, &h7, &hc } 

Sub render_bg() 
    Dim As UByte mode = disp_cnt.w And 7 

    Dim As ULong surf_addr = v_count.w * GBA_SCREEN_WIDTH * 4 

    Select Case As Const (mode)  
    	' BG Mode 1 - 240x160 pixels, Text and RS mode mixed
    	' BG Mode 2 - 240x160 pixels, RS mode
    	' BG Mode 3 - 240x160 pixels, 32768 colors
    	Case 0, 1, 2
            Dim As UByte enb = (disp_cnt.w Shr 8) And bg_enb(mode) 

            Dim As Byte prio, bg_idx 

            for prio = 3 To 0 Step -1         
                for bg_idx = 3 To 0 Step -1       
                	
                    If (enb And (1 Shl bg_idx))=0 Then continue For
                    If ((bg(bg_idx).ctrl.w And 3) <> prio) Then continue For

                    Dim As ULong  chr_base  = ((bg(bg_idx).ctrl.w Shr  2) And &h3)  Shl 14 
                    Dim As BOOL   is_256    =  (bg(bg_idx).ctrl.w Shr  7) And &h1 
                    Dim As UShort scrn_base = ((bg(bg_idx).ctrl.w Shr  8) And &h1f) Shl 11 
                    Dim As BOOL   aff_wrap  =  (bg(bg_idx).ctrl.w Shr 13) And &h1 
                    Dim As UShort scrn_size =  (bg(bg_idx).ctrl.w Shr 14) 

                    Dim As BOOL affine = iif(mode = 2,1,0) Or (IIf(mode = 1,1,0) And IIf(bg_idx = 2,1,0)) 

                    Dim As ULong ADDRESS = surf_addr 

                    If (affine) Then 
  
                        Dim As Short pa = bg_pa(bg_idx).w 
                        Dim As Short pb = bg_pb(bg_idx).w 
                        Dim As Short pc = bg_pc(bg_idx).w 
                        Dim As Short pd = bg_pd(bg_idx).w 

                        Dim As Long ox = (CLng(bg_refxi(bg_idx).w) Shl 4) Shr 4 
                        Dim As Long oy = (CLng(bg_refyi(bg_idx).w) Shl 4) Shr 4 

                        bg_refxi(bg_idx).w += pb 
                        bg_refyi(bg_idx).w += pd 

                        Dim As UByte tms = 16 Shl scrn_size 
                        Dim As UByte tmsk = tms - 1 

                        Dim As UByte x 

                        for x = 0 To GBA_SCREEN_WIDTH-1
                        	
                            Dim As Short tmx = ox Shr 11 
                            Dim As Short tmy = oy Shr 11 

                            if (aff_wrap) Then 
                                tmx And= tmsk 
                                tmy And= tmsk 
                            Else
                                If (tmx < 0) Or (tmx >= tms) Then GoTo ContinueFor2
                                if (tmy < 0) Or (tmy >= tms) Then GoTo ContinueFor2  
                            EndIf
  

                            Dim As UShort chr_x = (ox Shr 8) And 7 
                            Dim As UShort chr_y = (oy Shr 8) And 7 

                            Dim As ULong map_addr = scrn_base + tmy * tms + tmx 

                            Dim As ULong vram_addr = chr_base + vram[map_addr] * 64 + chr_y * 8 + chr_x 

                            Dim As UShort pal_idx = vram[vram_addr] 

                            if (pal_idx) Then RGB_to_mem(ADDRESS , palette_(pal_idx),1 ) '1=transparente
                            
                            
                        continuefor2:
                            ox+=pa
                            oy+=pc
                            ADDRESS+=4
                        Next

                    Else
         
                        Dim As UShort oy     = v_count.w + bg(bg_idx).yofs.w 
                        Dim As UShort tmy    = oy Shr 3 
                        Dim As UShort scrn_y = (tmy Shr 5) And 1 

                        Dim As UByte x 

                        for x = 0 To GBA_SCREEN_WIDTH-1         
                            Dim As UShort ox     = x + bg(bg_idx).xofs.w 
                            Dim As UShort tmx    = ox Shr 3 
                            Dim As UShort scrn_x = (tmx Shr 5) And 1 

                            Dim As UShort chr_x = ox And 7 
                            Dim As UShort chr_y = oy And 7 

                            Dim As UShort pal_idx 
                            Dim As UShort pal_base = 0 

                            Dim As ULong map_addr = scrn_base + (tmy And &h1f) * 32 * 2 + (tmx And &h1f) * 2 

                            Select Case As Const (scrn_size)  
                            	case 1  
                            		map_addr += scrn_x * 2048
                            	case 2  
                            		map_addr += scrn_y * 2048
                            	case 3  
                            		map_addr += scrn_x * 2048 + scrn_y * 4096
                            End Select

                            Dim As UShort tile = vram[map_addr + 0] Or (vram[map_addr + 1] Shl 8) 

                            Dim As UShort chr_numb = (tile Shr  0) And &h3ff 
                            Dim As BOOL   flip_x   = (tile Shr 10) And &h1 
                            Dim As BOOL   flip_y   = (tile Shr 11) And &h1 
                            Dim As UByte  chr_pal  = (tile Shr 12) And &hf 

                            if (is_256=0) Then pal_base = chr_pal * 16 

                            if (flip_x) Then chr_x Xor= 7 
                            if (flip_y) Then chr_y Xor= 7 

                            Dim As ULong vram_addr 

                            if (is_256) Then 
                                vram_addr = chr_base + chr_numb * 64 + chr_y * 8 + chr_x 
                                pal_idx   = vram[vram_addr] 
                            Else
                                vram_addr = chr_base + chr_numb * 32 + chr_y * 4 + (chr_x Shr 1) 
                                pal_idx   = (vram[vram_addr] Shr (chr_x And 1) * 4) And &hf 
                            EndIf
  
                            Dim As ULong pal_addr = pal_idx Or pal_base 

                            If (pal_idx) Then RGB_to_mem(ADDRESS , palette_(pal_addr)) 

                            ADDRESS += 4 
                        
                       Next  
                    EndIf
                Next
                render_obj(prio) 
            Next

		' BG Mode 3 - 240x160 pixels, 32768 colors            
    	Case 3  
            Dim As UByte x 
            Dim As ULong frm_addr = v_count.w * 480 

            for x = 0 To GBA_SCREEN_WIDTH-1        
                Dim As UShort pixel = vram[frm_addr + 0] Or (vram[frm_addr + 1] Shl 8) 

                Dim As UByte r = ((pixel Shr  0) And &h1f) Shl 3 
                Dim As UByte g = ((pixel Shr  5) And &h1f) Shl 3 
                Dim As UByte b = ((pixel Shr 10) And &h1f) Shl 3 

                Dim As ULong rgba_ = &hff 

                rgba_ Or= (r Or (r Shr 5)) Shl  8 
                rgba_ Or= (g Or (g Shr 5)) Shl 16 
                rgba_ Or= (b Or (b Shr 5)) Shl 24 

                RGB_to_mem(surf_addr , rgba_ )

                surf_addr += 4 

                frm_addr += 2 
           Next

		' BG Mode 4 - 240x160 pixels, 256 colors (out of 32768 colors)
    	Case 4  
            Dim As UByte x, frame = (disp_cnt.w Shr 4) And 1 
            Dim As ULong frm_addr = &ha000 * frame + v_count.w * GBA_SCREEN_WIDTH 

            for x = 0 To GBA_SCREEN_WIDTH-1         
                Dim As UByte pal_idx = vram[frm_addr] : frm_addr+=1
                RGB_to_mem(surf_addr , palette_(pal_idx) )
                surf_addr += 4 
            Next

		' BG Mode 5 - 160x128 pixels, 32768 colors
    	case 5
    		
            Dim As UByte x 
            Dim As ULong frm_addr = v_count.w * 960 

			  If v_count.w<GBA_MODE5_HEIGHT Then
	            for x = 0 To GBA_MODE5_WIDTH-1 
	                Dim As UShort pixel = vram[frm_addr + 0] Or (vram[frm_addr + 1] Shl 8) 
	
	                Dim As UByte r = ((pixel Shr  0) And &h1f) Shl 3 
	                Dim As UByte g = ((pixel Shr  5) And &h1f) Shl 3 
	                Dim As UByte b = ((pixel Shr 10) And &h1f) Shl 3 
	
	                Dim As ULong rgba_ = &hff 
	
	                rgba_ Or= (r Or (r Shr 5)) Shl  8 
	                rgba_ Or= (g Or (g Shr 5)) Shl 16 
	                rgba_ Or= (b Or (b Shr 5)) Shl 24 
	
	                RGB_to_mem(surf_addr , rgba_ )
	                
	                surf_addr += 4
	
	                frm_addr += 2
	           Next
           endif
            
            
    		
    	Case Else
    		Print "Modos 6 y 7 no existen en GBA":Sleep:end
    
   End Select

End Sub

Sub render_line() 
    Dim As ULong addr 

    Dim As ULong addr_s = v_count.w * GBA_SCREEN_WIDTH * 4 
    Dim As ULong addr_e = addr_s + GBA_SCREEN_WIDTH * 4 

    For addr = addr_s To addr_e -1 Step &h10      
        RGB_to_mem(addr Or &h0  , palette_(0) ) 
        RGB_to_mem(addr Or &h4  , palette_(0) ) 
        RGB_to_mem(addr Or &h8  , palette_(0) ) 
        RGB_to_mem(addr Or &hc  , palette_(0) ) 
    Next

    If ((disp_cnt.w And 7) > 2) Then 
        render_bg() 
        render_obj(0) 
        render_obj(1) 
        render_obj(2) 
        render_obj(3) 
    Else
        render_bg() 
    EndIf
  
End Sub

Sub vblank_start() 
    if (disp_stat.w And VBLK_IRQ) Then trigger_irq(VBLK_FLAG) 
    disp_stat.w Or= VBLK_FLAG 
End Sub

Sub hblank_start() 
    if (disp_stat.w And HBLK_IRQ) Then trigger_irq(HBLK_FLAG) 
    disp_stat.w Or= HBLK_FLAG 
End Sub

Sub vcount_match() 
    if (disp_stat.w And VCNT_IRQ) Then trigger_irq(VCNT_FLAG) 
    disp_stat.w Or= VCNT_FLAG 
End Sub

Sub run_frame() 
    disp_stat.w and= INV(VBLK_FLAG)
 
    v_count.w=0     
    for a As Integer = 0 To LINES_TOTAL-1         
        disp_stat.w And= INV(HBLK_FLAG Or VCNT_FLAG) 

        'V-Count match and V-Blank start
        if (v_count.w = disp_stat.b1) Then vcount_match() 

        If (v_count.w = LINES_VISIBLE) Then 
            bg_refxi(2).w = bg_refxe(2).w 
            bg_refyi(2).w = bg_refye(2).w 

            bg_refxi(3).w = bg_refxe(3).w 
            bg_refyi(3).w = bg_refye(3).w 

            vblank_start() 
            dma_transfer(_VBLANK)     
        EndIf

        arm_exec(CYC_LINE_HBLK0) 

        'H-Blank start
        If (v_count.w < LINES_VISIBLE) Then 
            render_line() 
            dma_transfer(_HBLANK) 
        EndIf

        hblank_start() 

        arm_exec(CYC_LINE_HBLK1) 

        sound_clock(CYC_LINE_TOTAL) 

        v_count.w+=1
    Next

	Sleep 10 ' necesario por ahora, pero tengo que estudiarlo

    memcpy(scrSurface->pixels, gbascreen, GBA_SCREEN_WIDTH * GBA_SCREEN_HEIGHT * 4) 
    SDL_BlitScaled(scrSurface, NULL, winsurface, NULL) 
    SDL_UpdateWindowSurface(sdlwindow)

   
    sound_buffer_wrap() 
End Sub
