

#define _EEPROM_WRITE  2
#define _EEPROM_READ   3

Dim Shared As ULong flash_bank= 0 

Enum flash_mode_e 
    _IDLE,
    _ERASE,
    _WRITE,
    _BANK_SWITCH
End enum 

Dim Shared As flash_mode_e flash_mode= _IDLE 

Dim Shared As BOOL flash_id_mode= FALSE 
Dim Shared As BOOL flash_used= FALSE 

Dim Shared As BOOL eeprom_used= FALSE 
Dim Shared As BOOL eeprom_read_stat= FALSE 

Dim Shared As ULong eeprom_addr= 0 
Dim Shared As ULong eeprom_addr_read= 0 

Dim Shared As UByte eeprom_buff(&hFF) 





Dim Shared As uByte bus_size_lut(15)  = { 4, 4, 2, 4, 4, 2, 2, 4, 2, 2, 2, 2, 2, 2, 1, 1 } 

Sub arm_access(ADDRESS As ULong , at As access_type_e) 
    Dim As UByte cycles = 1 

    if (ADDRESS And &h08000000) Then   
        if (at = _NON_SEQ) Then
            cycles += ws_n((ADDRESS Shr 25) And 3) 
        Else
            cycles += ws_s((ADDRESS Shr 25) And 3)
        EndIf
    ElseIf  ((ADDRESS Shr 24) = 2) Then 
        cycles += 2 
    EndIf
  
    arm_cycles += cycles 
End Sub

Sub arm_access_bus(ADDRESS As ULong , size As uByte , at As access_type_e) 
    Dim As UByte lut_idx = (ADDRESS Shr 24) And &hf 
    Dim As UByte bus_sz = bus_size_lut(lut_idx) 

    if (bus_sz < size) Then 
        arm_access(ADDRESS + 0, at) 
        arm_access(ADDRESS + 2, _SEQUENTIAL) 
    Else
        arm_access(ADDRESS, at) 
    EndIf
  
End Sub




'Memory read
Function bios_read(ADDRESS As ULong) As uByte 
    if ((ADDRESS Or arm_r.r(15)) < &h4000) Then 
        return bios[ADDRESS And &h3fff] 
    Else
        return CUByte(bios_op)
    EndIf
End Function

Function wram_read(ADDRESS As ULong) As uByte 
    return wram[ADDRESS And &h3ffff] 
End Function

Function iwram_read(ADDRESS As ULong) As uByte 
    return iwram[ADDRESS And &h7fff] 
End Function

Function pram_read(ADDRESS As ULong) As uByte 
    return pram[ADDRESS And &h3ff] 
End Function

Function vram_read(ADDRESS As ULong) As uByte 
    return vram[ADDRESS And iif(ADDRESS And &h10000 , &h17fff , &h1ffff)] 
End Function

Function oam_read(ADDRESS As ULong) As uByte 
    return oam[ADDRESS And &h3ff] 
End Function

Function rom_read(ADDRESS As ULong) As uByte 
    return rom[ADDRESS And cart_rom_mask] 
End Function

Function eeprom_read(ADDRESS As ULong , offset As uByte) As uByte 
	
	' sistema original, no me gusta, lo cambio por otro mas intuitivo
    'if (eeprom_used<>0) And _
    '	 (  ((cart_rom_size >  &h1000000) And ((ADDRESS Shr 8)  = &h0dffff)) _
    '	Or  ((cart_rom_size <= &h1000000) And ((ADDRESS Shr 24) = &h00000d))  ) Then
    
    Dim As Integer a=IIf((cart_rom_size >  &h1000000) And ((ADDRESS Shr 8 ) = &h0dffff) ,1,0)
    Dim As Integer b=IIf((cart_rom_size <= &h1000000) And ((ADDRESS Shr 24) = &h00000d) ,1,0)
    a=a Or b
      
    if (eeprom_used<>0) And (a<>0) Then
         if offset=0 Then 
             Dim As UByte mode = eeprom_buff(0) Shr 6 
             Select Case As const (mode)  
             	case _EEPROM_WRITE  
             		return 1 
             	case _EEPROM_READ  
                    Dim As UByte value = 0 
                    if (eeprom_idx >= 4) Then 
                        Dim As UByte idx  = ((eeprom_idx - 4) Shr 3) And 7 
                        Dim As UByte bit_ = ((eeprom_idx - 4) Shr 0) And 7 
                        value = (eeprom[eeprom_addr_read Or idx] Shr (bit_ Xor 7)) And 1 
                    EndIf
                    eeprom_idx+=1  
                    return value 
             End Select
         EndIf
    Else
    	   ' en caso de error, se devuelve contenido de ROM, no de EEPROM
        	Return rom_read(ADDRESS )
    EndIf

    return 0 
End Function

Function flash_read(ADDRESS As ULong) As uByte 
    if (flash_id_mode) Then 
    	'This is the Flash ROM ID, we return Sanyo ID code
        Select Case As Const (ADDRESS)  
        	case &h0e000000  
        		return &h62 
        	case &h0e000001  
        		return &h13 
        End Select
    ElseIf (flash_used) Then
        return flash[flash_bank Or (ADDRESS And &hffff)] 
    Else
        return sram[ADDRESS And &hffff] 
    EndIf

    return 0 
End Function


#define IS_OPEN_BUS(a)  iif( (((a) Shr 28)<>0)  Or  (  (((a) >= &h00004000)<>0) And  (((a) < &h02000000))<>0),1,0)

Function arm_readb(ADDRESS As ULong) As uByte 
    Dim As UByte value = arm_read_(ADDRESS, 0) 

    if (ADDRESS And &h08000000)=0 Then 
        io_open_bus And= iif((ADDRESS Shr 24) = 4,1,0) 
        if (IS_OPEN_BUS(ADDRESS)<>0) Or (io_open_bus <>0) Then 
            value = arm_pipe(1)
        EndIf
    EndIf
  
    return value 
End Function

Function arm_readh(ADDRESS As ULong) As ULong 
    Dim As ULong a = ADDRESS And INV(1) 
    Dim As UByte s = ADDRESS And 1 

    Dim As ULong value = _
        (arm_read_(a Or 0, 0) Shl 0) Or _
        (arm_read_(a Or 1, 1) Shl 8) 

    if (a And &h08000000)=0 Then 
        io_open_bus And= IIf((a Shr 24) = 4,1,0) 
        if (a < &h4000) And (arm_r.r(15) >= &h4000) Then 
            value = bios_op     And &hffff 
        ElseIf (IS_OPEN_BUS(a)<>0) Or (io_open_bus<>0) Then
            value = arm_pipe(1) And &hffff
        EndIf
    EndIf
  
    return ROR(value, s Shl 3) 
End Function

Function arm_read(ADDRESS As ULong) As ULong 
    Dim As ULong a = ADDRESS And  INV(3)
    Dim As UByte s = ADDRESS And 3 

    Dim As ULong value = _
        (arm_read_(a Or 0, 0) Shl  0) Or _
        (arm_read_(a Or 1, 1) Shl  8) Or _
        (arm_read_(a Or 2, 2) Shl 16) Or _
        (arm_read_(a Or 3, 3) Shl 24) 

    if (a And &h08000000)=0 Then 
        io_open_bus And= iif((a Shr 24) = 4,1,0) 
        if (a < &h4000) And (arm_r.r(15) >= &h4000) Then 
            value = bios_op 
        ElseIf  (IS_OPEN_BUS(a)<>0) Or (io_open_bus<>0) Then
            value = arm_pipe(1)
        EndIf
    EndIf
  
    return ROR(value, s Shl 3) 
End Function

Function arm_readb_n(ADDRESS As ULong) As uByte 
    arm_access_bus(ADDRESS, _ARM_BYTE_SZ, _NON_SEQ) 
    return arm_readb(ADDRESS) 
End Function

Function arm_readh_n(ADDRESS As ULong) As ULong 
    arm_access_bus(ADDRESS, _ARM_HWORD_SZ, _NON_SEQ) 
    return arm_readh(ADDRESS) 
End Function

Function arm_read_n(ADDRESS As ULong) As ULong 
    arm_access_bus(ADDRESS, _ARM_WORD_SZ, _NON_SEQ) 
    return arm_read(ADDRESS) 
End Function

Function arm_readb_s(ADDRESS As ULong) As uByte 
    arm_access_bus(ADDRESS, _ARM_BYTE_SZ, _SEQUENTIAL) 
    return arm_readb(ADDRESS) 
End Function

Function arm_readh_s(ADDRESS As ULong) As ULong 
    arm_access_bus(ADDRESS, _ARM_HWORD_SZ, _SEQUENTIAL) 
    return arm_readh(ADDRESS) 
End Function

Function arm_read_s(ADDRESS As ULong) As ULong 
    arm_access_bus(ADDRESS, _ARM_WORD_SZ, _SEQUENTIAL) 
    return arm_read(ADDRESS) 
End Function





'Memory write
Sub wram_write(ADDRESS As ULong , value As uByte) 
    wram[ADDRESS And &h3ffff] = value 
End Sub

Sub iwram_write(ADDRESS As ULong , value As uByte) 
    iwram[ADDRESS And &h7fff] = value 
End Sub

Sub pram_write(ADDRESS As ULong , value As uByte) 
    pram[ADDRESS And &h3ff] = value 

    ADDRESS And= &h3fe 

    Dim As UShort pixel = pram[ADDRESS] Or (pram[ADDRESS + 1] Shl 8) 

    Dim As UByte r = ((pixel Shr  0) And &h1f) Shl 3 
    Dim As UByte g = ((pixel Shr  5) And &h1f) Shl 3 
    Dim As UByte b = ((pixel Shr 10) And &h1f) Shl 3 

    Dim As ULong rgba_ = &hff 

    rgba_ Or= (r Or (r Shr 5)) Shl  8 
    rgba_ Or= (g Or (g Shr 5)) Shl 16 
    rgba_ Or= (b Or (b Shr 5)) Shl 24 

    palette_(ADDRESS Shr 1) = rgba_ 
End Sub

Sub vram_write(ADDRESS As ULong , value As uByte) 
    vram[ADDRESS And IIf(ADDRESS And &h10000 , &h17fff , &h1ffff)] = value 
End Sub

Sub oam_write(ADDRESS As ULong , value As uByte) 
    oam[ADDRESS And &h3ff] = value 
End Sub

Sub eeprom_write(ADDRESS As ULong , offset As uByte , value As uByte) 

	' sistema original, no me gusta, lo cambio por otro mas intuitivo
    'If (offset=0) And _
    '     ( ((cart_rom_size >  &h1000000) And ((ADDRESS Shr 8 ) = &h0dffff)) _
    '   Or  ((cart_rom_size <= &h1000000) And ((ADDRESS Shr 24) = &h00000d)) ) Then
  
    Dim As Integer a=IIf((cart_rom_size >  &h1000000) And ((ADDRESS Shr 8 ) = &h0dffff) ,1,0)
    Dim As Integer b=IIf((cart_rom_size <= &h1000000) And ((ADDRESS Shr 24) = &h00000d) ,1,0)
    a=a Or b
      
    If (offset=0) And (a<>0) Then

        if (eeprom_idx = 0) Then 
        	'First write, erase buffer
            eeprom_read_stat = FALSE 
            Dim As UShort i 
            for  i = 0 To &hFF       
                eeprom_buff(i) = 0
            Next
        EndIf
  
        Dim As UByte idx  = (eeprom_idx Shr 3) And &hff 
        Dim As UByte bit_ = (eeprom_idx Shr 0) And &h7 

        eeprom_buff(idx) Or= (value And 1) Shl (bit_ Xor 7) 

		  eeprom_idx+=1
        if (eeprom_idx = dma_ch(3).count.w) Then 
            'Last write, process buffer
            Dim As UByte mode = eeprom_buff(0) Shr 6 
            'Value is only valid if bit 1 is set (2 or 3, READ = 2)
            if (mode And _EEPROM_READ) Then 
                Dim As BOOL eep_512b = IIf( eeprom_idx = (2 + 6 + IIf(mode = _EEPROM_WRITE , 64 , 0) + 1)  ,1,0) 
            
                if (eep_512b) Then 
                    eeprom_addr =   eeprom_buff(0) And &h3f 
                Else
                    eeprom_addr = ((eeprom_buff(0) And &h3f) Shl 8) Or eeprom_buff(1)
                EndIf

                eeprom_addr Shl = 3 

                if (mode = _EEPROM_WRITE) Then 
                    'Perform write to actual EEPROM buffer
                    Dim As UByte buff_addr = IIf(eep_512b , 1 , 2)        

						  ' la escritura en EEPROM se realiza en 64bits (8 bytes) a la vez
                    *(eeprom + eeprom_addr + 0) = eeprom_buff(buff_addr + 0) 
                    *(eeprom + eeprom_addr + 1) = eeprom_buff(buff_addr + 1) 
                    *(eeprom + eeprom_addr + 2) = eeprom_buff(buff_addr + 2) 
                    *(eeprom + eeprom_addr + 3) = eeprom_buff(buff_addr + 3) 
                    *(eeprom + eeprom_addr + 4) = eeprom_buff(buff_addr + 4) 
                    *(eeprom + eeprom_addr + 5) = eeprom_buff(buff_addr + 5) 
                    *(eeprom + eeprom_addr + 6) = eeprom_buff(buff_addr + 6) 
                    *(eeprom + eeprom_addr + 7) = eeprom_buff(buff_addr + 7) 

                Else
                    eeprom_addr_read = eeprom_addr 
                EndIf
  
                eeprom_idx = 0 
            EndIf
        EndIf
        
        eeprom_used = TRUE 
    EndIf
  
End Sub

Sub flash_write(ADDRESS As ULong , value As uByte) 
	
    if (flash_mode = _WRITE) Then   
        flash[flash_bank Or (ADDRESS And &hffff)] = value 
        flash_mode = _IDLE 
    ElseIf  (flash_mode = _BANK_SWITCH) And (ADDRESS = &h0e000000) Then
        flash_bank = (value And 1) Shl 16 
        flash_mode = _IDLE 
    ElseIf  (sram[&h5555] = &haa) And (sram[&h2aaa] = &h55) Then
        if (ADDRESS = &h0e005555) Then 'Command to do something on Flash ROM
            Select Case As Const (value) 'Erase all
               case &h10 
                    if (flash_mode = _ERASE) Then
                        Dim As ULong idx 
                        for idx = 0 To &h1FFFF         
                            flash[idx] = &hff 
                        Next
                        flash_mode = _IDLE 
                    EndIf
            	case &h80  
            		flash_mode    = _ERASE
            	case &h90  
            		flash_id_mode = TRUE
            	case &ha0  
            		flash_mode    = _WRITE
            	case &hb0  
            		flash_mode    = _BANK_SWITCH
            	case &hf0  
            		flash_id_mode = FALSE 
            
            End Select

            'We try to guess that the game uses flash if it
            'writes the specific flash commands to the save memory region
            if (flash_mode<>0) Or (flash_id_mode<>0) Then flash_used = TRUE 

        ElseIf  (flash_mode = _ERASE) And (value = &h30) Then
            Dim As ULong bank_s = ADDRESS And &hf000 
            Dim As ULong bank_e = bank_s + &h1000 
            Dim As ULong idx 
            for idx = bank_s To bank_e-1         
                flash[flash_bank Or idx] = &hff 
            Next
            flash_mode = _IDLE 
        EndIf
    EndIf

    sram[ADDRESS And &hffff] = value 
End Sub


Sub arm_writeb(ADDRESS As ULong , value As uByte) 
    Dim As UByte ah = (ADDRESS Shr 24)

    if (ah = 7) Then Exit Sub  'OAM doesn´t supposrt 8 bits writes

    if (ah > 4) And (ah < 8) Then 
        arm_write_(ADDRESS + 0, 0, value) 
        arm_write_(ADDRESS + 1, 1, value) 
    Else
        arm_write_(ADDRESS, 0, value) 
    EndIf
  
End Sub

Sub arm_writeh(ADDRESS As ULong , value As uShort) 
    Dim As ULong a = ADDRESS And &hFFFFFFFE 'INV(1) 

    arm_write_(a Or 0, 0, (value Shr 0) And &hFF)
    arm_write_(a Or 1, 1, (value Shr 8) And &hFF)
End Sub

Sub arm_write(ADDRESS As ULong , value As ULong) 
    Dim As ULong a = ADDRESS And &hFFFFFFFC 'INV(3) 

    arm_write_(a Or 0, 0, (value Shr  0) And &hFF) 
    arm_write_(a Or 1, 1, (value Shr  8) And &hFF) 
    arm_write_(a Or 2, 2, (value Shr 16) And &hFF) 
    arm_write_(a Or 3, 3, (value Shr 24) And &hFF) 
End Sub

Sub arm_writeb_n(ADDRESS As ULong , value As uByte) 
    arm_access_bus(ADDRESS, _ARM_BYTE_SZ, _NON_SEQ) 

    arm_writeb(ADDRESS, value) 
End Sub

Sub arm_writeh_n(ADDRESS As ULong , value As uShort) 
    arm_access_bus(ADDRESS, _ARM_HWORD_SZ, _NON_SEQ) 

    arm_writeh(ADDRESS, value) 
End Sub

Sub arm_write_n(ADDRESS As ULong , value As ULong) 
    arm_access_bus(ADDRESS, _ARM_WORD_SZ, _NON_SEQ) 

    arm_write(ADDRESS, value) 
End Sub

Sub arm_writeb_s(ADDRESS As ULong , value As uByte) 
    arm_access_bus(ADDRESS, _ARM_BYTE_SZ, _SEQUENTIAL) 

    arm_writeb(ADDRESS, value) 
End Sub

Sub arm_writeh_s(ADDRESS As ULong , value As uShort) 
    arm_access_bus(ADDRESS, _ARM_HWORD_SZ, _SEQUENTIAL) 

    arm_writeh(ADDRESS, value) 
End Sub

Sub arm_write_s(ADDRESS As ULong , value As ULong) 
    arm_access_bus(ADDRESS, _ARM_WORD_SZ, _SEQUENTIAL) 

    arm_write(ADDRESS, value) 
End Sub








' direcciones mapa de memoria

Function arm_read_(ADDRESS As ULong , offset As uByte) As uByte 

    Select Case As Const (ADDRESS Shr 24)  
    	case &h0  
    		return bios_read(ADDRESS)   ' bios 16k
    	case &h2  
    		return wram_read(ADDRESS)   ' interna 256k
    	case &h3  
    		return iwram_read(ADDRESS)  ' interna 32k
    	case &h4  
    		return io_read(ADDRESS)     ' I/O 1k
    	case &h5  
    		return pram_read(ADDRESS)   ' paleta colores 1k
    	case &h6  
    		return vram_read(ADDRESS)   ' video 96k
    	case &h7  
    		return oam_read(ADDRESS)    ' objetos 1k
    	case &h8 ,&h9
         Return rom_read(ADDRESS)    ' cartuchos 32mb
    	case &ha ,&hb
         Return rom_read(ADDRESS)    ' espejo cartuchos 32mb
    	case &hc ,&hd
         Return eeprom_read(ADDRESS, offset) ' salvaguarda de juegos en cartucho
    	case &he ,&hf
         Return flash_read(ADDRESS)   ' salvaguarda de juegos en cartucho (nota: incluye SRAM)
    End Select

    return 0 
End Function

Sub arm_write_(ADDRESS As ULong , offset As uByte , value As uByte) 
    Select Case As Const (ADDRESS Shr 24) And &HFF
    	case &h2  
    		wram_write(ADDRESS, value)   ' interna 256k
    	case &h3  
    		iwram_write(ADDRESS, value)  ' interna 32k
    	case &h4 
    		io_write(ADDRESS, value)     ' I/O 1k
    	case &h5  
    		pram_write(ADDRESS, value)   ' paleta colores 1k
    	case &h6   
    		vram_write(ADDRESS, value)   ' video 96k
    	case &h7  
    		oam_write(ADDRESS, value)    ' objetos 1k
    	Case &hc,&hd 
         eeprom_write(ADDRESS, offset, value)  ' salvaguarda de juegos en cartucho
    	Case &he ,&hf 
         flash_write(ADDRESS, value)  ' salvaguarda de juegos en cartucho (nota: incluye SRAM)
    End Select

End Sub









Sub ARM_deb()
  'If arm_r.r(15)=&h0800084c Then DEB=3 ' thum test 225
  'If arm_r.r(15)=&h08000a9c Then DEB=3 ' arm  test 225
  'If arm_r.r(15)=&h080000F4 Then DEB=3 ' sram
  'If arm_r.r(15)=&h080b16ec And arm_r.r(3)=&h03007e12 Then DEB=3 ' sram
  'If arm_r.r(15)=&h0807D21A  Then DEB=3 ' sram
  'If DEB Then 
	'	If arm_r.r(1)=&h200 And arm_r.r(3)=&h03007e12 Then Sleep
  'EndIf
  'If arm_r.r(1)=&h200 And arm_r.r(3)=&h03007e12 Then DEB=3 ' sram
  'If arm_r.r(0)=16 And arm_r.r(8)=32 Then DEB=3
  'If aaa=2 Then DEB=3
  
	If DEB Then
	  'If DEB=1 Then Locate 1,1
	  Print "----------------------------------------------------------------------------------------------------------"
	  Print "DIR:";hex(arm_r.r(15),8);" ";
	  Print "   CPSR:";hex( arm_r.cpsr, 8)
     'Print "   SPSR:";hex( arm_r.spsr_fiq, 8)
     '
	  'Print "NZVC - IFTS"
     'print gba_cpu_flagN ;
     'print gba_cpu_flagZ ;
     'print gba_cpu_flagV ;
     'print gba_cpu_flagC ;" - ";
     'print gba_cpu_flagI ;
     'print gba_cpu_flagF ;
     'print gba_cpu_flagT ;
     'print gba_cpu_shifterCarry
     
     Print " r0:";hex( arm_r.r(0), 8);
     Print " r1:";hex( arm_r.r(1), 8);
     Print "  r2:";hex( arm_r.r(2), 8);
     Print "  r3:";hex( arm_r.r(3), 8);
     Print "  r4:";hex( arm_r.r(4), 8);
     Print "  r5:";hex( arm_r.r(5), 8);
     Print "  r6:";hex( arm_r.r(6), 8);
     Print "  r7:";hex( arm_r.r(7), 8)
     Print " r8:";hex( arm_r.r(8), 8);
     Print " r9:";hex( arm_r.r(9), 8);
     Print " r10:";hex( arm_r.r(10), 8);
     Print " r11:";hex( arm_r.r(11), 8);
     Print " r12:";hex( arm_r.r(12), 8);
     Print " r13:";hex( arm_r.r(13), 8);
     Print " r14:";hex( arm_r.r(14), 8);
     Print " r15:";hex( arm_r.r(15), 8)
     Print
     Print "OPCODE:";Hex(DEB_opcode,8)
     If DEB=3 Then Sleep
	EndIf

End Sub