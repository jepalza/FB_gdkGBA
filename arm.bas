
/'
 * Utils
 '/

Sub arm_flag_set(flag As ULong , cond As BOOL) 
    if (cond<>0) Then 
        arm_r.cpsr Or= flag 
    Else
        arm_r.cpsr And= INV(flag)
    EndIf
End Sub

Sub arm_bank_to_regs(mode As Byte) 
    if (mode <> _ARM_FIQ) Then 
        arm_r.r(8)  = arm_r.r8_usr 
        arm_r.r(9)  = arm_r.r9_usr 
        arm_r.r(10) = arm_r.r10_usr 
        arm_r.r(11) = arm_r.r11_usr 
        arm_r.r(12) = arm_r.r12_usr 
    EndIf
  
    Select Case As Const (mode)  
    	case _ARM_USR ,_ARM_SYS 
            arm_r.r(13) = arm_r.r13_usr 
            arm_r.r(14) = arm_r.r14_usr 

    	case _ARM_FIQ 
            arm_r.r(8)  = arm_r.r8_fiq 
            arm_r.r(9)  = arm_r.r9_fiq 
            arm_r.r(10) = arm_r.r10_fiq 
            arm_r.r(11) = arm_r.r11_fiq 
            arm_r.r(12) = arm_r.r12_fiq 
            arm_r.r(13) = arm_r.r13_fiq 
            arm_r.r(14) = arm_r.r14_fiq 

    	case _ARM_IRQ 
            arm_r.r(13) = arm_r.r13_irq 
            arm_r.r(14) = arm_r.r14_irq 

    	case _ARM_SVC 
            arm_r.r(13) = arm_r.r13_svc 
            arm_r.r(14) = arm_r.r14_svc 

    	case _ARM_MON 
            arm_r.r(13) = arm_r.r13_mon 
            arm_r.r(14) = arm_r.r14_mon 

    	case _ARM_ABT 
            arm_r.r(13) = arm_r.r13_abt 
            arm_r.r(14) = arm_r.r14_abt 

    	case _ARM_UND 
            arm_r.r(13) = arm_r.r13_und 
            arm_r.r(14) = arm_r.r14_und 
    
    End Select

End Sub

Sub arm_regs_to_bank(mode As Byte) 
    if (mode <> _ARM_FIQ) Then 
        arm_r.r8_usr  = arm_r.r(8) 
        arm_r.r9_usr  = arm_r.r(9) 
        arm_r.r10_usr = arm_r.r(10) 
        arm_r.r11_usr = arm_r.r(11) 
        arm_r.r12_usr = arm_r.r(12) 
    EndIf
  
    Select Case As Const (mode)  
    	case _ARM_USR ,_ARM_SYS 
            arm_r.r13_usr = arm_r.r(13) 
            arm_r.r14_usr = arm_r.r(14) 

    	case _ARM_FIQ 
            arm_r.r8_fiq  = arm_r.r(8) 
            arm_r.r9_fiq  = arm_r.r(9) 
            arm_r.r10_fiq = arm_r.r(10) 
            arm_r.r11_fiq = arm_r.r(11) 
            arm_r.r12_fiq = arm_r.r(12) 
            arm_r.r13_fiq = arm_r.r(13) 
            arm_r.r14_fiq = arm_r.r(14) 

    	case _ARM_IRQ 
            arm_r.r13_irq = arm_r.r(13) 
            arm_r.r14_irq = arm_r.r(14) 

    	case _ARM_SVC 
            arm_r.r13_svc = arm_r.r(13) 
            arm_r.r14_svc = arm_r.r(14) 

    	case _ARM_MON 
            arm_r.r13_mon = arm_r.r(13) 
            arm_r.r14_mon = arm_r.r(14) 

    	case _ARM_ABT 
            arm_r.r13_abt = arm_r.r(13) 
            arm_r.r14_abt = arm_r.r(14) 

    	Case _ARM_UND 
            arm_r.r13_und = arm_r.r(13) 
            arm_r.r14_und = arm_r.r(14) 
    
    End Select

End Sub

Sub arm_mode_set(mode As Byte) 
    Dim As Byte curr = arm_r.cpsr And &h1f 

    arm_r.cpsr And= INV(&h1f)
    arm_r.cpsr Or = mode 

    arm_regs_to_bank(curr) 
    arm_bank_to_regs(mode) 
End Sub

Function arm_flag_tst(flag As ULong) As BOOL 
    return IIf((arm_r.cpsr And flag)<>0,1,0)
End Function

Function arm_cond(cond As Byte) As BOOL 
    Dim As BOOL res 

    Dim As BOOL n = arm_flag_tst(_ARM_N) 
    Dim As BOOL z = arm_flag_tst(_ARM_Z) 
    Dim As BOOL c = arm_flag_tst(_ARM_C) 
    Dim As BOOL v = arm_flag_tst(_ARM_V) 

    Select Case As Const (cond Shr 1)  
    	case 0  
    		res = z
    	case 1  
    		res = c 
    	case 2  
    		res = n
    	case 3  
    		res = v 
    	case 4  
    		res = IIf((c<>0) And (z=0),1,0)
    	case 5  
    		res = IIf(n = v,1,0)
    	case 6  
    		res = IIf((z=0) And (n = v),1,0)
    	case else  
    		res = TRUE
    End Select

    if (cond And 1) Then res = IIf(res=0,1,0) 

    return res 
End Function

Sub arm_spsr_get( psr As ULong Ptr) 

    Select Case As Const (arm_r.cpsr And &h1f)  
    	case _ARM_FIQ  
    		*psr = arm_r.spsr_fiq
    	case _ARM_IRQ  
    		*psr = arm_r.spsr_irq
    	case _ARM_SVC  
    		*psr = arm_r.spsr_svc 
    	case _ARM_MON  
    		*psr = arm_r.spsr_mon 
    	case _ARM_ABT  
    		*psr = arm_r.spsr_abt
    	case _ARM_UND  
    		*psr = arm_r.spsr_und 
    End Select

End Sub

Sub arm_spsr_set(spsr As ULong) 
	
    Select Case As Const (arm_r.cpsr And &h1f)  
    	case _ARM_FIQ  
    		arm_r.spsr_fiq = spsr
    	case _ARM_IRQ  
    		arm_r.spsr_irq = spsr 
    	case _ARM_SVC  
    		arm_r.spsr_svc = spsr
    	case _ARM_MON  
    		arm_r.spsr_mon = spsr 
    	case _ARM_ABT  
    		arm_r.spsr_abt = spsr 
    	case _ARM_UND  
    		arm_r.spsr_und = spsr
    End Select

End Sub

Sub arm_spsr_to_cpsr() 
    Dim As Byte curr = arm_r.cpsr And &h1f 

    arm_spsr_get(@arm_r.cpsr) 

    Dim As Byte mode = arm_r.cpsr And &h1f 

    arm_regs_to_bank(curr) 
    arm_bank_to_regs(mode) 
End Sub

Sub arm_setn(res As ULong) 
    arm_flag_set(_ARM_N, IIf(res And (1 Shl 31),1,0)) 
End Sub

Sub arm_setn64(res As uLongInt) 
    arm_flag_set(_ARM_N, IIf(res And (1 Shl 63),1,0))
End Sub

Sub arm_setz(res As ULong) 
    arm_flag_set(_ARM_Z, IIf(res = 0,1,0)) 
End Sub

Sub arm_setz64(res As uLongInt) 
    arm_flag_set(_ARM_Z, IIf(res = 0,1,0)) 
End Sub

Sub arm_addc(res As uLongInt) 
    arm_flag_set(_ARM_C, IIf(res > &hffffffff,1,0)) 
End Sub

Sub arm_subc(res As uLongInt) 
    arm_flag_set(_ARM_C, IIf(res < &h100000000,1,0))
End Sub

Sub arm_addv(lhs As ULong , rhs As ULong , res As ULong) 
    arm_flag_set(_ARM_V, IIf(INV(lhs Xor rhs) And (lhs Xor res) And &h80000000 ,1,0))
End Sub

Sub arm_subv(lhs As ULong , rhs As ULong , res As ULong) 
    arm_flag_set(_ARM_V, IIf((lhs Xor rhs) And (lhs Xor res) And &h80000000 ,1,0))
End Sub

Function arm_saturate(valor As LongInt , min_ As Long , max_ As Long , q As BOOL) As ULong 
    Dim As ULong res = CULng(valor)

    if (valor > max_) Or (valor < min_) Then 
  
        if (valor > max_) Then  
            res = culng(max_)
        Else
            res = culng(min_)
        EndIf

        if (q) Then arm_flag_set(_ARM_Q, TRUE) 
    EndIf

    return res 
End Function

Function arm_ssatq(valor As LongInt) As ULong 
    return arm_saturate(valor, &h80000000, &h7fffffff, TRUE) 
End Function

Function arm_in_thumb() As BOOL 
    return arm_flag_tst(_ARM_T) 
End Function

Function arm_fetchh(at As access_type_e) As uShort 
	Dim As UShort Ptr twobyte

    Select Case As Const (arm_r.r(15) Shr 24)  
    	case &h0  
    		arm_cycles += 1
    		twobyte = (bios + (arm_r.r(15) And &h3fff))
    		bios_op = *twobyte
    		Return bios_op
    	case &h2  
    		arm_cycles += 3
    		twobyte=(wram  + (arm_r.r(15) And &h3ffff))
    		Return *twobyte
    	case &h3  
    		arm_cycles += 1
    		twobyte=(iwram + (arm_r.r(15) And &h7fff))
    		Return *twobyte
    	case &h5  
    		arm_cycles += 1
    		twobyte=(pram  + (arm_r.r(15) And &h3ff))
    		Return *twobyte
    	case &h7  
    		arm_cycles += 1
    		twobyte=(oam   + (arm_r.r(15) And &h3ff))
    		Return *twobyte

    	Case &h8 ,&h9 
            if (at = _NON_SEQ) Then 
                arm_cycles += ws_n_t16(0) 
            Else
                arm_cycles += ws_s_t16(0)
            EndIf
          	twobyte=(rom + (arm_r.r(15) And &h1ffffff))
    			Return *twobyte

    	Case &ha ,&hb 
            if (at = _NON_SEQ) Then 
                arm_cycles += ws_n_t16(1) 
            Else
                arm_cycles += ws_s_t16(1)
            EndIf
            twobyte=(rom + (arm_r.r(15) And &h1ffffff))
            Return *twobyte

    	Case &hc,&hd 
            if (at = _NON_SEQ) Then 
                arm_cycles += ws_n_t16(2) 
            Else
                arm_cycles += ws_s_t16(2)
            EndIf
            twobyte=(rom + (arm_r.r(15) And &h1ffffff))
            Return *twobyte

    	Case else 
            if (at = _NON_SEQ) Then 
                return arm_readh_n(arm_r.r(15)) 
            Else
                return arm_readh_s(arm_r.r(15))
            EndIf
  
    End Select

End Function

Function arm_fetch(at As access_type_e) As ULong 
    Dim As ULong Ptr fourbyte
    
    Select Case As Const (arm_r.r(15) Shr 24)  
    	case &h0  
    		arm_cycles += 1
    		fourbyte=(bios + (arm_r.r(15) And &h3fff))
    		bios_op = *fourbyte
    		return bios_op
    	case &h2  
    		arm_cycles += 6
    		fourbyte=(wram + (arm_r.r(15) And &h3ffff))
    		Return *fourbyte
    	case &h3  
    		arm_cycles += 1
    		fourbyte=(iwram + (arm_r.r(15) And &h7fff))
    		Return *fourbyte
    	case &h5  
    		arm_cycles += 1
    		fourbyte=(pram + (arm_r.r(15) And &h3ff))
    		Return *fourbyte
    	case &h7  
    		arm_cycles += 1
    		fourbyte=(oam + (arm_r.r(15) And &h3ff))
    		Return *fourbyte
    	Case &h8 ,&h9 
         if (at = _NON_SEQ) Then 
            arm_cycles += ws_n_arm(0) 
         Else
            arm_cycles += ws_s_arm(0)
         EndIf
         fourbyte=(rom + (arm_r.r(15) And &h1ffffff))
         Return *fourbyte
    	Case &ha ,&hb 
         if (at = _NON_SEQ) Then 
            arm_cycles += ws_n_arm(1) 
         Else
            arm_cycles += ws_s_arm(1)
         EndIf
         fourbyte=(rom + (arm_r.r(15) And &h1ffffff))
         Return *fourbyte
    	case &hc ,&hd
         if (at = _NON_SEQ) Then 
            arm_cycles += ws_n_arm(2) 
         Else
            arm_cycles += ws_s_arm(2)
         EndIf
         fourbyte=(rom + (arm_r.r(15) And &h1ffffff))
         Return *fourbyte
    	Case else 
         if (at = _NON_SEQ) Then 
            return arm_read_n(arm_r.r(15)) 
         Else
            return arm_read_s(arm_r.r(15))
         EndIf
    End Select

End Function

Function arm_fetch_n() As ULong 
    Dim As ULong op 

    if (arm_in_thumb()) Then 
        op = arm_fetchh(_NON_SEQ) 
        arm_r.r(15) += 2 
    Else
        op = arm_fetch(_NON_SEQ)
        arm_r.r(15) += 4 
    EndIf
  
    return op 
End Function

Function arm_fetch_s() As ULong 
    Dim As ULong op 

    if (arm_in_thumb()) Then 
        op = arm_fetchh(_SEQUENTIAL) 
        arm_r.r(15) += 2 
    Else
        op = arm_fetch(_SEQUENTIAL) 
        arm_r.r(15) += 4 
    EndIf
  
    return op 
End Function

Sub arm_load_pipe() 
    arm_pipe(0) = arm_fetch_n() 
    arm_pipe(1) = arm_fetch_s() 

    pipe_reload = TRUE 
End Sub

Sub arm_r15_align() 
    if (arm_in_thumb()) Then 
        arm_r.r(15) And= INV(1) 
    Else
        arm_r.r(15) And= INV(3)
    EndIf
End Sub

Sub arm_interwork() 
    arm_flag_set(_ARM_T, arm_r.r(15) And 1) 

    arm_r15_align() 
    arm_load_pipe() 
End Sub

Sub arm_cycles_s_to_n() 
    if (arm_r.r(15) And &h08000000) Then 
        Dim As UByte idx = (arm_r.r(15) Shr 25) And 3 
        if (arm_in_thumb()) Then 
            arm_cycles -= ws_s_t16(idx) 
            arm_cycles += ws_n_t16(idx) 
        Else
            arm_cycles -= ws_s_arm(idx) 
            arm_cycles += ws_n_arm(idx) 
        EndIf
    EndIf
End Sub




' * Execute * //Data Processing (Arithmetic)

Sub arm_arith_set(op As arm_data_t , res As uLongInt , add_ As BOOL) 
    if (op.rd = 15) Then   
        if (op.s) Then
            arm_spsr_to_cpsr() 
            arm_r15_align() 
            arm_load_pipe() 
            arm_check_irq() 
        Else
            arm_r15_align() 
            arm_load_pipe() 
        EndIf
    ElseIf (op.s) Then 
        arm_setn(res) 
        arm_setz(res) 
        if (add_) Then 
            arm_addc(res) 
            arm_addv(op.lhs, op.rhs, res) 
        Else
            arm_subc(res) 
            arm_subv(op.lhs, op.rhs, res) 
        EndIf
    EndIf
  
End Sub

sub arm_arith_add(op as arm_data_t , adc as BOOL) 
If DEB=3 Then Print "arm_arith_add"
    dim as ulongint res = op.lhs + op.rhs 
    if (adc) then res += arm_flag_tst(_ARM_C) 

    arm_r.r(op.rd) = res 

    arm_arith_set(op, res, _ARM_ARITH_ADD) 
end sub

sub arm_arith_subtract(op as arm_data_t , sbc as BOOL , rev as BOOL) 
If DEB=3 Then Print "arm_arith_subtract"
    if (rev) then 
        dim as ulongint tmp = op.lhs 
        op.lhs = op.rhs 
        op.rhs = tmp 
    endif

    dim as ulongint res = op.lhs - op.rhs 

    if (sbc) then res -= iif(arm_flag_tst(_ARM_C)=0,1,0)':print "revisar arm_arith_subtract"
    
    arm_r.r(op.rd) = res 
    arm_arith_set(op, res, _ARM_ARITH_SUB) 
end sub

sub arm_arith_rsb(op as arm_data_t) 
If DEB=3 Then Print "arm_arith_rsb"
    arm_arith_subtract(op, _ARM_ARITH_NO_C, _ARM_ARITH_REVERSE) 
end sub

sub arm_arith_rsc(op as arm_data_t) 
If DEB=3 Then Print "arm_arith_rsc"
    arm_arith_subtract(op, _ARM_ARITH_CARRY, _ARM_ARITH_REVERSE) 
end sub

sub arm_arith_sbc(op as arm_data_t) 
If DEB=3 Then Print "arm_arith_sbc"
    arm_arith_subtract(op, _ARM_ARITH_CARRY, _ARM_ARITH_NO_REV) 
end sub

sub arm_arith_sub(op as arm_data_t) 
If DEB=3 Then Print "arm_arith_sub"
    arm_arith_subtract(op, _ARM_ARITH_NO_C, _ARM_ARITH_NO_REV) 
end sub

sub arm_arith_cmn(op as arm_data_t) 
If DEB=3 Then Print "arm_arith_cmn"
    arm_arith_set(op, op.lhs + op.rhs, _ARM_ARITH_ADD) 
end sub

sub arm_arith_cmp(op as arm_data_t) 
If DEB=3 Then Print "arm_arith_cmp"
    arm_arith_set(op, op.lhs - op.rhs, _ARM_ARITH_SUB) 
end sub


' Data Processing (Logical)

sub arm_logic_set(op as arm_data_t , res as ulong) 
If DEB=3 Then Print "arm_logic_set"

    If (op.rd = 15) then   
        if (op.s) then
            arm_spsr_to_cpsr() 
            arm_r15_align() 
            arm_load_pipe() 
            arm_check_irq() 
        else
            arm_r15_align() 
            arm_load_pipe() 
        EndIf
    ElseIf (op.s) then 
        arm_setn(res) 
        arm_setz(res) 
        arm_flag_set(_ARM_C, op.cout) 
    EndIf
  
end sub

sub arm_logic(op as arm_data_t , inst as arm_logic_e) 
If DEB=3 Then Print "arm_logic"
    dim as ulong res 

    select case as const (inst)  
    	case ARD ' AND
    		res = op.lhs and op.rhs 
    	case BIC
    		res = op.lhs and INV(op.rhs) 
    	case EOR
    		res = op.lhs xor op.rhs
    	case ORN
    		res = op.lhs or INV(op.rhs)
    	case ORR
    		res = op.lhs or op.rhs
    	case MVN
    		res = INV(op.rhs) 
    	case SHF 'shift
    		res = op.rhs
    end select

    arm_r.r(op.rd) = res 
    arm_logic_set(op, res) 
end sub





'--------------------------
' ASR,ROR,LSL,LSR
sub arm_asr(op as arm_data_t) 
If DEB=3 Then Print "arm_asr"
    dim as longint valor = CLng(op.lhs) 
    dim as ubyte sh = op.rhs 

    if (sh > 32) then sh = 32 

    ' posible problema si "sh=0" genera un OP.COUT=FALSE, cuando deberia ser TRUE. lo arreglo con un "if (sh)"
    ' lo he detectado en los test THUMB, al dar error 68 en SHIFTS.ASM
	 op.rhs  = valor shr sh 
	 If (sh) Then
	   op.cout = IIf(valor and (1 shl (sh - 1)),1,0) 
	 Else
	   op.cout=TRUE ' esto evita que salga FALSE
	 EndIf

    arm_r.r(op.rd) = op.rhs 
    arm_logic_set(op, op.rhs) 
end sub

sub arm_lsl(op as arm_data_t) 
If DEB=3 Then Print "arm_lsl"
    dim as ulongint valor = op.lhs 
    dim as ubyte sh = op.rhs 

    dim as BOOL c = arm_flag_tst(_ARM_C) 

    op.rhs = valor 
    op.cout = c

    if (sh > 32) then   
        op.rhs = 0 
        op.cout = 0 
    elseif (sh) then
        op.rhs = valor shl sh 
        op.cout = IIf(valor and (1 shl (32 - sh)),1,0) 
    endif

    arm_r.r(op.rd) = op.rhs 
    arm_logic_set(op, op.rhs) 
end sub

sub arm_lsr(op as arm_data_t) 
If DEB=3 Then Print "arm_lsr"
    dim as ulongint valor = op.lhs 
    dim as ubyte sh = op.rhs 
   
    dim as BOOL c = arm_flag_tst(_ARM_C) 

    op.rhs = valor 
    op.cout = c 

    if (sh > 32) then   
        op.rhs = 0 
        op.cout = 0 
    elseif (sh) then
        op.rhs = valor shr sh 
        op.cout = IIf(valor and (1 shl (sh - 1)),1,0) 
    endif

    arm_r.r(op.rd) = op.rhs 
    arm_logic_set(op, op.rhs) 
end sub

sub arm_ror(op as arm_data_t) 
If DEB=3 Then Print "arm_ror"
    dim as ulongint valor = op.lhs 
    dim as ubyte sh = op.rhs 

    dim as BOOL c = arm_flag_tst(_ARM_C) 

    op.rhs = valor 
    op.cout = c 

    if (sh) then 
        sh and= &h1f 
        if (sh = 0) then sh = 32 
        op.rhs = ROR(valor, sh) 
        op.cout = IIf(valor and (1 shl (sh - 1)),1,0) 
    endif
    
    arm_r.r(op.rd) = op.rhs 
    arm_logic_set(op, op.rhs) 
end sub
' -----------------------------------







sub arm_count_zeros(rd as ubyte) 
If DEB=3 Then Print "arm_count_zeros"
    dim as ubyte rm = arm_op and &hf 
    dim as ulong m = arm_r.r(rm) 
    dim as ulong i, cnt = 32 

    for i = 0 to 31         
        if (m and (1 shl i)) then cnt = i xor &h1f 
    next

    arm_r.r(rd) = cnt 
end sub

sub arm_logic_teq(op as arm_data_t) 
If DEB=3 Then Print "arm_logic_teq"
    arm_logic_set(op, op.lhs xor op.rhs) 
end sub

sub arm_logic_tst(op as arm_data_t) 
If DEB=3 Then Print "arm_logic_tst"
    arm_logic_set(op, op.lhs and op.rhs) 
end sub



' Data Processing Operand Decoding
function arm_data_imm_op() as arm_data_t 
If DEB=3 Then Print "arm_data_imm_op"
    dim as ulong imm   = (arm_op shr  0) and &hff 
    dim as ubyte shift = (arm_op shr  7) and &h1e 
    dim as ubyte rd    = (arm_op shr 12) and &h0f 
    dim as ubyte rn    = (arm_op shr 16) and &h0f 

    imm = ROR(imm, shift) 

    dim as arm_data_t op
        op.rd=rd
        op.lhs=arm_r.r(rn)
        op.rhs=imm
        op.cout=IIf(imm and (1 shl 31),1,0)
        op.s=SBIT(arm_op) 

    return op 
end function

function arm_data_regi(rm as ubyte , tipo as ubyte , imm as ubyte) as arm_shifter_t 
If DEB=3 Then Print "arm_data_regi"
    dim as arm_shifter_t sal 

    sal.valor = arm_r.r(rm) 
    sal.cout  = arm_flag_tst(_ARM_C) 

    dim as ulong m = sal.valor 
    dim as ubyte c = sal.cout 

    select case as const (tipo)  
    	case 0  'lsl
            if (imm) then 
                sal.valor = m shl imm 
                sal.cout = IIf(m and (1 shl (32 - imm)) ,1,0)
            endif

    	case 1,_  'lsr
    	     2    'asr
            if (imm = 0) then imm = 32 
            if (tipo = 2) then 
                sal.valor = clngint(clng(m)) shr imm
            else
                sal.valor = culngint(m) shr imm
            endif

            sal.cout = IIf(m and (1 shl (imm - 1)),1,0) 

    	case 3  'ROR
            if (imm) then 
                sal.valor = ROR(m, imm) 
                sal.cout  = IIf(m and (1 shl (imm - 1)) ,1,0)
              else 'rrx
                sal.valor = ROR((m and INV(1)) or c, 1) 
                sal.cout  = m and 1 
            endif
    
    end select

    return sal 
end Function

function arm_data_regi_op() as arm_data_t 
If DEB=3 Then Print "arm_data_regi_op"
    dim as ubyte rm   = (arm_op shr  0) and &hf 
    dim as ubyte tipo = (arm_op shr  5) and &h3 
    dim as ubyte imm  = (arm_op shr  7) and &h1f 
    dim as ubyte rd   = (arm_op shr 12) and &hf 
    dim as ubyte rn   = (arm_op shr 16) and &hf 

    dim as arm_shifter_t shift = arm_data_regi(rm, tipo, imm) 

    dim as arm_data_t op
        op.rd=rd
        op.lhs=arm_r.r(rn)
        op.rhs=shift.valor
        op.cout=shift.cout
        op.s=SBIT(arm_op)
    

    return op 
end function

function arm_data_regr(rm as ubyte , tipo as ubyte , rs as ubyte) as arm_shifter_t 
If DEB=3 Then Print "arm_data_regr"
    dim as arm_shifter_t sal 

    sal.valor = arm_r.r(rm) 
    sal.cout = arm_flag_tst(_ARM_C) 

    if (rm = 15) then sal.valor += 4 

    dim as ubyte sh = arm_r.r(rs) 

    if (sh >= 32) then 
        if (tipo = 3) then sh and= &h1f 
        if (tipo = 2) or (sh = 0) then sh = 32 

        sal.valor = 0 
        sal.cout = 0 
    endif
  

    if (sh<>0) and (sh <= 32) then 
        dim as ulong m = arm_r.r(rm) 
        select case as const (tipo)  
        	case 0  'lsl
                sal.valor = culngint(m) shl sh
                sal.cout =IIf( m and (1 shl (32 - sh)) ,1,0)

        	case 1, _  'lsr
        	     2     'asr
                if (tipo = 2) then 
                    sal.valor = clngint(clng(m)) shr sh
                else
                    sal.valor = culngint(m) shr sh
                endif
                sal.cout = IIf(m and (1 shl (sh - 1)),1,0) 

        	case 3  'ROR
                sal.valor = ROR(culngint(m), sh) 
                sal.cout = IIf(m and (1 shl (sh - 1)),1,0) 
        
        end select
    endif

    arm_cycles+=1  
    arm_cycles_s_to_n() 

    return sal 
end Function

function arm_data_regr_op() as arm_data_t 
If DEB=3 Then Print "arm_data_regr_op"
    dim as ubyte rm   = (arm_op shr  0) and &hf 
    dim as ubyte tipo = (arm_op shr  5) and &h3 
    dim as ubyte rs   = (arm_op shr  8) and &hf 
    dim as ubyte rd   = (arm_op shr 12) and &hf 
    dim as ubyte rn   = (arm_op shr 16) and &hf 

    dim as arm_shifter_t shift = arm_data_regr(rm, tipo, rs) 

    dim as arm_data_t dato
        dato.rd=rd
        dato.lhs=arm_r.r(rn)
        dato.rhs=shift.valor
        dato.cout=shift.cout
        dato.s=SBIT(arm_op)
    

    return dato
end function

function t16_data_imm3_op() as arm_data_t 
If DEB=3 Then Print "t16_data_imm3_op"
    dim as ubyte rd  = (arm_op shr 0) and &h7 
    dim as ubyte rn  = (arm_op shr 3) and &h7 
    dim as ubyte imm = (arm_op shr 6) and &h7 

    dim as arm_data_t op
        op.rd=rd
        op.lhs=arm_r.r(rn)
        op.rhs=imm
        op.cout=FALSE
        op.s=TRUE
    
    return op 
end function

function t16_data_imm8_op() as arm_data_t 
If DEB=3 Then Print "t16_data_imm8_op"
    dim as ubyte imm = (arm_op shr 0) and &hff 
    dim as ubyte rdn = (arm_op shr 8) and &h7 

    dim as arm_data_t op
        op.rd=rdn
        op.lhs=arm_r.r(rdn)
        op.rhs=imm
        op.cout=FALSE
        op.s=TRUE
    

    return op 
end function

function t16_data_rdn3_op() as arm_data_t 
If DEB=3 Then Print "t16_data_rdn3_op"
    dim as ubyte rdn = (arm_op shr 0) and 7 
    dim as ubyte rm  = (arm_op shr 3) and 7 

    dim as arm_data_t op
        op.rd=rdn
        op.lhs=arm_r.r(rdn)
        op.rhs=arm_r.r(rm)
        op.cout=FALSE
        op.s=TRUE
    

    return op 
end function

function t16_data_neg_op() as arm_data_t 
If DEB=3 Then Print "t16_data_neg_op"
    dim as ubyte rd = (arm_op shr 0) and 7 
    dim as ubyte rm = (arm_op shr 3) and 7 

    dim as arm_data_t op
        op.rd=rd
        op.lhs=0
        op.rhs=arm_r.r(rm)
        op.cout=FALSE
        op.s=TRUE
    

    return op 
end function

function t16_data_reg_op() as arm_data_t 
If DEB=3 Then Print "t16_data_reg_op"
    dim as ubyte rd = (arm_op shr 0) and 7 
    dim as ubyte rn = (arm_op shr 3) and 7 
    dim as ubyte rm = (arm_op shr 6) and 7 

    dim as arm_data_t op
        op.rd=rd
        op.lhs=arm_r.r(rn)
        op.rhs=arm_r.r(rm)
        op.cout=FALSE
        op.s=TRUE
    

    return op 
end function

function t16_data_rdn4_op(s as BOOL) as arm_data_t 
If DEB=3 Then Print "t16_data_rdn4_op"
    dim as ubyte rm = (arm_op shr 3) and &hf 

    dim as ubyte rdn 

    rdn   = (arm_op shr 0) and &h7 
    rdn or= (arm_op shr 4) and &h8 

    dim as arm_data_t op
        op.rd=rdn
        op.lhs=arm_r.r(rdn)
        op.rhs=arm_r.r(rm)
        op.cout=FALSE
        op.s=s
    

    return op 
end function

function t16_data_imm7sp_op() as arm_data_t 
If DEB=3 Then Print "t16_data_imm7sp_op"
    dim as ushort imm = (arm_op and &h7f) shl 2 

    dim as arm_data_t op
        op.rd=13
        op.lhs=arm_r.r(13)
        op.rhs=imm
        op.cout=FALSE
        op.s=TRUE
    

    return op 
end function

function t16_data_imm8sp_op() as arm_data_t 
If DEB=3 Then Print "t16_data_imm8sp_op"
    dim as arm_data_t op = t16_data_imm8_op() 

    op.lhs = arm_r.r(13) 
    op.rhs shl = 2 
    op.s = FALSE 

    return op 
end function

function t16_data_imm8pc_op() as arm_data_t 
If DEB=3 Then Print "t16_data_imm8pc_op"
    dim as arm_data_t op = t16_data_imm8_op() 

    op.lhs = arm_r.r(15) and INV(3) 
    op.rhs shl = 2 
    op.s = FALSE 

    return op 
end function

function t16_data_imm5_op(rsh as BOOL) as arm_data_t 
If DEB=3 Then Print "t16_data_imm5_op"
    dim as ubyte rd  = (arm_op shr 0) and &h7 
    dim as ubyte rn  = (arm_op shr 3) and &h7 
    dim as ubyte imm = (arm_op shr 6) and &h1f 

    dim as arm_data_t op
        op.rd=rd
        op.lhs=arm_r.r(rn)
        op.rhs=iif((rsh<>0) and (imm = 0) , 32 , imm)
        op.cout=FALSE
        op.s=TRUE

    return op 
end function



' Multiplication
sub arm_mpy_inc_cycles(rhs as ulongint , u as BOOL) 
If DEB=3 Then Print "arm_mpy_inc_cycles"
    dim as ulong m1 = rhs and &hffffff00 
    dim as ulong m2 = rhs and &hffff0000 
    dim as ulong m3 = rhs and &hff000000 

    if      (m1 = 0) or (((u=0) and (m1 = &hffffff00))<>0) then 
        arm_cycles += 1 
    elseif  (m2 = 0) or (((u=0) and (m2 = &hffff0000))<>0) then
        arm_cycles += 2 
    elseif  (m3 = 0) or (((u=0) and (m3 = &hff000000))<>0) then
        arm_cycles += 3 
    else
        arm_cycles += 4
    endif

    arm_cycles_s_to_n() 
end sub

sub arm_mpy_add(op as arm_mpy_t) 
If DEB=3 Then Print "arm_mpy_add"
    dim as ulong a = arm_r.r(op.ra) 

    dim as ulong res = a + op.lhs * op.rhs 

    arm_r.r(op.rd) = res 
    if (op.s) then 
        arm_setn(res) 
        arm_setz(res) 
    endif

    arm_mpy_inc_cycles(op.rhs, _ARM_MPY_SIGNED) 
    arm_cycles+=1  
end sub

sub arm_mpy(op as arm_mpy_t) 
If DEB=3 Then Print "arm_mpy"
    dim as ulong res = op.lhs * op.rhs 

    arm_r.r(op.rd) = res 

    if (op.s) then 
        arm_setn(res) 
        arm_setz(res) 
    endif

    arm_mpy_inc_cycles(op.rhs, _ARM_MPY_SIGNED) 
end sub

sub arm_mpy_smla__(op as arm_mpy_t , m as BOOL , n as BOOL) 
If DEB=3 Then Print "arm_mpy_smla__"
    if (m) then op.lhs shr = 16 
    if (n) then op.rhs shr = 16 

    dim as LongInt l = CShort(op.lhs) 
    dim as LongInt r = cshort(op.rhs) 

    dim as LongInt res = arm_r.r(op.ra) + l * r 

    arm_r.r(op.rd) = res 

    dim as long d = arm_r.r(op.rd) 

    if (d <> res) then arm_flag_set(_ARM_Q, TRUE) 

    arm_mpy_inc_cycles(op.rhs, _ARM_MPY_SIGNED) 

    arm_cycles+=1  
end sub

sub arm_mpy_smlal(op as arm_mpy_t) 
If DEB=3 Then Print "arm_mpy_smlal"
    dim as LongInt l = CLng(op.lhs) 
    dim as LongInt r = CLng(op.rhs) 

    dim as LongInt a = arm_r.r(op.ra) 
    dim as LongInt d = arm_r.r(op.rd) 

    a or= d shl 32 

    dim as LongInt res = a + l * r 

    arm_r.r(op.ra) = res 
    arm_r.r(op.rd) = res shr 32 

    if (op.s) then 
        arm_setn64(res) 
        arm_setz64(res) 
    endif

    arm_mpy_inc_cycles(op.rhs, _ARM_MPY_SIGNED) 

    arm_cycles += 2 
end sub

sub arm_mpy_smlal__(op as arm_mpy_t , m as BOOL , n as BOOL) 
If DEB=3 Then Print "arm_mpy_smlal__"
    if (m) then op.lhs shr = 16 
    if (n) then op.rhs shr = 16 

    dim as LongInt l = CShort(op.lhs) 
    dim as LongInt r = CShort(op.rhs) 

    dim as LongInt a = arm_r.r(op.ra) 
    dim as LongInt d = arm_r.r(op.rd) 

    a or= d shl 32 

    dim as LongInt res = a + l * r 

    arm_r.r(op.ra) = res 
    arm_r.r(op.rd) = res shr 32 

    arm_mpy_inc_cycles(op.rhs, _ARM_MPY_SIGNED) 

    arm_cycles += 2 
end sub

sub arm_mpy_smlaw_(op as arm_mpy_t , m as BOOL) 
If DEB=3 Then Print "arm_mpy_smlaw_"
    if (m) then op.lhs shr = 16 

    dim as LongInt l = CShort(op.lhs) 
    dim as LongInt a = arm_r.r(op.ra) 

    dim as LongInt res = (a shl 16) + l * op.rhs 

    arm_r.r(op.rd) = res shr 16 

    dim as long d = CLng(arm_r.r(op.rd)) 

    if (d <> res) then arm_flag_set(_ARM_Q, TRUE) 

    arm_mpy_inc_cycles(op.rhs, _ARM_MPY_SIGNED) 

    arm_cycles+=1  
end Sub

sub arm_mpy_smul(op as arm_mpy_t , m as BOOL , n as BOOL) 
If DEB=3 Then Print "arm_mpy_smul"
    if (m) then op.lhs shr = 16 
    if (n) then op.rhs shr = 16 

    dim as LongInt l = CShort(op.lhs) 
    dim as LongInt r = CShort(op.rhs) 

    dim as LongInt res = l * r 

    arm_r.r(op.rd) = res 

    arm_mpy_inc_cycles(op.rhs, _ARM_MPY_SIGNED) 
end sub

sub arm_mpy_smull(op as arm_mpy_t) 
If DEB=3 Then Print "arm_mpy_smull"
    dim as LongInt l = CLng(op.lhs) 
    dim as LongInt r = CLng(op.rhs) 

    dim as LongInt res = l * r 

    arm_r.r(op.ra) = res 
    arm_r.r(op.rd) = res shr 32 

    if (op.s) then 
        arm_setn64(res) 
        arm_setz64(res) 
    endif

    arm_mpy_inc_cycles(op.rhs, _ARM_MPY_SIGNED) 

    arm_cycles+=1  
end sub

sub arm_mpy_smulw_(op as arm_mpy_t , m as BOOL) 
If DEB=3 Then Print "arm_mpy_smulw_"
    if (m) then op.lhs shr = 16 

    dim as LongInt res =  CShort(op.lhs) * op.rhs 

    arm_r.r(op.rd) = res shr 16 

    arm_mpy_inc_cycles(op.rhs, _ARM_MPY_SIGNED) 
end sub

sub arm_mpy_umlal(op as arm_mpy_t) 
If DEB=3 Then Print "arm_mpy_umlal"
    dim as ULongInt l = op.lhs 
    dim as ULongInt r = op.rhs 

    dim as ULongInt a = arm_r.r(op.ra) 
    dim as ULongInt d = arm_r.r(op.rd) 

    a or= d shl 32 

    dim as ULongInt res = a + l * r 

    arm_r.r(op.ra) = res 
    arm_r.r(op.rd) = res shr 32 

    if (op.s) then 
        arm_setn64(res) 
        arm_setz64(res) 
    endif
  
    arm_mpy_inc_cycles(op.rhs, _ARM_MPY_UNSIGN) 

    arm_cycles += 2 
end sub

sub arm_mpy_umull(op as arm_mpy_t) 
If DEB=3 Then Print "arm_mpy_umull"
    dim as uLongInt l = op.lhs 
    dim as ULongInt r = op.rhs 

    dim as ULongInt res = l * r 

    arm_r.r(op.ra) = res 
    arm_r.r(op.rd) = res shr 32 

    if (op.s) then 
        arm_setn64(res) 
        arm_setz64(res) 
    endif

    arm_mpy_inc_cycles(op.rhs, _ARM_MPY_UNSIGN) 

    arm_cycles+=1  
end sub

function arm_mpy_op() as arm_mpy_t 
If DEB=3 Then Print "arm_mpy_op"
    dim as ubyte rn = (arm_op shr  0) and &hf 
    dim as ubyte rm = (arm_op shr  8) and &hf 
    dim as ubyte ra = (arm_op shr 12) and &hf 
    dim as ubyte rd = (arm_op shr 16) and &hf 
    dim as BOOL  s  = (arm_op shr 20) and &h1 

    dim as arm_mpy_t op =  type ( _
			 arm_r.r(rn), _
			 arm_r.r(rm), _
			 ra, _
			 rd, _
			 s _
    )

    return op 
end function

function t16_mpy_op() as arm_mpy_t 
If DEB=3 Then Print "t16_mpy_op"
    dim as ubyte rdn = (arm_op shr 0) and 7 
    dim as ubyte rm  = (arm_op shr 3) and 7 

    dim as arm_mpy_t op 
			 op.rd =rdn
			 op.lhs=arm_r.r(rdn)
			 op.rhs=arm_r.r(rm)
			 op.s  =TRUE
    
    return op 
end function



' Processor State
sub arm_psr_to_reg(op as arm_psr_t) 
If DEB=3 Then Print "arm_psr_to_reg"
    if (op.r) then 
        arm_spsr_get(@arm_r.r(op.rd)) 
    else
        if ((arm_r.cpsr and &h1f) = _ARM_USR) then 
            arm_r.r(op.rd) = arm_r.cpsr and USR_MASK 
        else
            arm_r.r(op.rd) = arm_r.cpsr and PRIV_MASK
        endif
    endif
  
end sub

sub arm_reg_to_psr(op as arm_psr_t) 
If DEB=3 Then Print "arm_reg_to_psr"
    dim as ulong spsr = 0 
    dim as ulong mask = 0 

    if (op.mask and 1) then mask   = &h000000ff 
    if (op.mask and 2) then mask or= &h0000ff00 
    if (op.mask and 4) then mask or= &h00ff0000 
    if (op.mask and 8) then mask or= &hff000000 

    dim as ulong sec_msk 

    if ((arm_r.cpsr and &h1f) = _ARM_USR) then 
        sec_msk = USR_MASK 
    else
        sec_msk = PRIV_MASK
    endif

    if (op.r) then sec_msk or= STATE_MASK 

    mask and= sec_msk 

    op.psr and= mask 

    if (op.r) then 
        arm_spsr_get(@spsr) 
        arm_spsr_set((spsr and INV(mask)) or op.psr) 
    else
        dim as byte curr = arm_r.cpsr and &h1f 
        dim as byte mode = op.psr     and &h1f 

        arm_r.cpsr and= INV(mask) 
        arm_r.cpsr Or = op.psr 

        arm_regs_to_bank(curr) 
        arm_bank_to_regs(mode) 

        arm_check_irq() 
    endif
  
end sub

function arm_mrs_op() as arm_psr_t 
If DEB=3 Then Print "arm_mrs_op"
    dim as arm_psr_t op  
			 op.rd=(arm_op shr 12) and &hf
			 op.r =(arm_op shr 22) and &h1

    return op 
end function

function arm_msr_imm_op() as arm_psr_t 
If DEB=3 Then Print "arm_msr_imm_op"
    dim as arm_data_t dato = arm_data_imm_op() 

    dim as arm_psr_t op
			 op.psr=dato.rhs
			 op.mask=(arm_op shr 16) and &hf
			 op.r=(arm_op shr 22) and &h1 
    
    return op 
end function

function arm_msr_reg_op() as arm_psr_t 
If DEB=3 Then Print "arm_msr_reg_op"
    dim as ubyte rm   = (arm_op shr  0) and &hf 
    dim as ubyte mask = (arm_op shr 16) and &hf 
    dim as BOOL  r    = (arm_op shr 22) and &h1 

    dim as arm_psr_t op
			 op.psr=arm_r.r(rm)
			 op.mask=mask
			 op.r=r
   
    return op 
end function



' Load/Store
function arm_memio_reg_get(reg as ubyte) as ulong 
If DEB=3 Then Print "arm_memio_reg_get"

    if (reg = 15) then 
        return arm_r.r(15) + 4 
    else
        return arm_r.r(reg)
    endif
  
end function

sub arm_memio_ldm(op as arm_memio_t) 
If DEB=3 Then Print "arm_memio_ldm"
    dim as ubyte i 

    arm_r.r(op.rn) += op.disp 

    for i = 0 to 15         
        if (op.regs and (1 shl i)) then 
            arm_r.r(i) = arm_read_s(op.addr) 
            op.addr += 4 
        endif
    next

    if (op.regs and &h8000) then 
        arm_r15_align() 
        arm_load_pipe() 
    endif
  
    arm_cycles+=1  

    arm_cycles_s_to_n() 
end sub

sub arm_memio_ldm_usr(op as arm_memio_t) 
If DEB=3 Then Print "arm_memio_ldm_usr"
    if (arm_op and &h8000) then 
        arm_memio_ldm(op) 
        arm_spsr_to_cpsr() 
        arm_check_irq() 
    else
        dim as byte mode = arm_r.cpsr and &h1f 
        arm_mode_set(_ARM_USR) 
        arm_memio_ldm(op) 
        arm_mode_set(mode) 
    endif
  
end sub

sub arm_memio_stm(op as arm_memio_t) 
If DEB=3 Then Print "arm_memio_stm"
    dim as BOOL first = TRUE 
    dim as ubyte i 

    for i = 0 to 15         
        if (op.regs and (1 shl i)) then 
            arm_write_s(op.addr, arm_memio_reg_get(i)) 
            if (first) then 
                arm_r.r(op.rn) += op.disp 
                first = FALSE 
            endif
            op.addr += 4 
        endif
    next

    arm_cycles_s_to_n() 
end sub

sub arm_memio_stm_usr(op as arm_memio_t) 
If DEB=3 Then Print "arm_memio_stm_usr"
    dim as byte mode = arm_r.cpsr and &h1f 

    arm_mode_set(_ARM_USR) 
    arm_memio_stm(op) 
    arm_mode_set(mode) 
end sub

sub arm_memio_ldr(op as arm_memio_t) 
If DEB=3 Then Print "arm_memio_ldr"
    arm_r.r(op.rt) = arm_read_n(op.addr) 

    if (op.rt = 15) then 
        arm_r15_align() 
        arm_load_pipe() 
    endif
  
    arm_cycles+=1  

    arm_cycles_s_to_n() 
end sub

sub arm_memio_ldrb(op as arm_memio_t) 
If DEB=3 Then Print "arm_memio_ldrb"
    arm_r.r(op.rt) = arm_readb_n(op.addr) 

    if (op.rt = 15) then 
        arm_r15_align() 
        arm_load_pipe() 
    endif
  
    arm_cycles+=1  

    arm_cycles_s_to_n() 
end sub

sub arm_memio_ldrh(op as arm_memio_t) 
If DEB=3 Then Print "arm_memio_ldrh"
    arm_r.r(op.rt) = arm_readh_n(op.addr) 

    if (op.rt = 15) then 
        arm_r15_align() 
        arm_load_pipe() 
    endif
 
    arm_cycles+=1  

    arm_cycles_s_to_n() 
end sub

sub arm_memio_ldrsb(op as arm_memio_t) 
If DEB=3 Then Print "arm_memio_ldrsb"
    dim as long value = arm_readb_n(op.addr) 

    value shl = 24 
    value shr = 24 

    arm_r.r(op.rt) = value 

    if (op.rt = 15) then 
        arm_r15_align() 
        arm_load_pipe() 
    endif
  
    arm_cycles+=1  

    arm_cycles_s_to_n() 
end sub

sub arm_memio_ldrsh(op as arm_memio_t) 
If DEB=3 Then Print "arm_memio_ldrsh"
    dim as long value = arm_readh_n(op.addr) 

    value shl = iif(op.addr and 1 , 24 , 16) 
    value shr = iif(op.addr and 1 , 24 , 16) 

    arm_r.r(op.rt) = value 

    if (op.rt = 15) then 
        arm_r15_align() 
        arm_load_pipe() 
    endif

    arm_cycles+=1  

    arm_cycles_s_to_n() 
end sub

sub arm_memio_ldrd(op as arm_memio_t) 
If DEB=3 Then Print "arm_memio_ldrd"
    arm_r.r(op.rt)  = arm_read_n(op.addr + 0) 
    arm_r.r(op.rt2) = arm_read_s(op.addr + 4) 

    if (op.rt2 = 15) then 
        arm_r15_align() 
        arm_load_pipe() 
    endif

    arm_cycles+=1  

    arm_cycles_s_to_n() 
end sub

sub arm_memio_str(op as arm_memio_t) 
If DEB=3 Then Print "arm_memio_str"
    arm_write_n(op.addr, arm_memio_reg_get(op.rt)) 

    arm_cycles_s_to_n() 
end sub

sub arm_memio_strb(op as arm_memio_t) 
If DEB=3 Then Print "arm_memio_strb"
    arm_writeb_n(op.addr, arm_memio_reg_get(op.rt)) 

    arm_cycles_s_to_n() 
end sub

sub arm_memio_strh(op as arm_memio_t) 
If DEB=3 Then Print "arm_memio_strh"
    arm_writeh_n(op.addr, arm_memio_reg_get(op.rt)) 

    arm_cycles_s_to_n() 
end sub

sub arm_memio_strd(op as arm_memio_t) 
If DEB=3 Then Print "arm_memio_strd"
    arm_write_n(op.addr + 0, arm_memio_reg_get(op.rt)) 
    arm_write_s(op.addr + 4, arm_memio_reg_get(op.rt2)) 

    arm_cycles_s_to_n() 
end sub

sub arm_memio_load_usr(op as arm_memio_t , size as arm_size_e) 
If DEB=3 Then Print "arm_memio_load_usr"
    dim as byte mode = arm_r.cpsr and &h1f 

    arm_mode_set(_ARM_USR) 

    select case as const (size)  
    	case _BYTE   
    		arm_memio_ldrb(op)
    	case _HWORD  
    		arm_memio_ldrh(op)
    	case _WORD   
    		arm_memio_ldr (op) 
    	case _DWORD  
    		arm_memio_ldrd(op)
    end select

    arm_mode_set(mode) 
end sub

sub arm_memio_store_usr(op as arm_memio_t , size as arm_size_e) 
If DEB=3 Then Print "arm_memio_store_usr"
    dim as byte mode = arm_r.cpsr and &h1f 

    arm_mode_set(_ARM_USR) 

    select case as const (size)  
    	case _BYTE   
    		arm_memio_strb(op)
    	case _HWORD  
    		arm_memio_strh(op) 
    	case _WORD   
    		arm_memio_str (op) 
    	case _DWORD  
    		arm_memio_strd(op)
    end select

    arm_mode_set(mode) 
end sub

sub arm_memio_ldrbt(op as arm_memio_t) 
If DEB=3 Then Print "arm_memio_ldrbt"
    arm_memio_load_usr(op, _BYTE) 
end sub

sub arm_memio_strbt(op as arm_memio_t) 
If DEB=3 Then Print "arm_memio_strbt"
    arm_memio_store_usr(op, _BYTE) 
end sub

function arm_memio_mult_op() as arm_memio_t 
If DEB=3 Then Print "arm_memio_mult_op"
    dim as ushort regs = (arm_op shr  0) and &hffff 
    dim as ubyte  rn   = (arm_op shr 16) and &hf 
    dim as BOOL   w    = (arm_op shr 21) and &h1 
    dim as BOOL   u    = (arm_op shr 23) and &h1 
    dim as BOOL   p    = (arm_op shr 24) and &h1 

    dim as ubyte i, cnt = 0 

    for i = 0 to 15        
        if (regs and (1 shl i)) then cnt += 4 
    next

    dim as arm_memio_t op
			 op.regs=regs
			 op.rn=rn
			 op.addr=arm_r.r(rn) and INV(3)
    

    if (u = 0) then op.addr -= cnt 
    if (u = p) then op.addr += 4 

    if (w) then 
        if (u) then 
            op.disp =  cnt 
        else
            op.disp = -cnt
        endif
    else
        op.disp = 0 
    endif

    return op 
end function

function arm_memio_imm_op() as arm_memio_t 
If DEB=3 Then Print "arm_memio_imm_op"
    dim as ushort imm = (arm_op shr  0) and &hfff 
    dim as ubyte  rt  = (arm_op shr 12) and &hf 
    dim as ubyte  rn  = (arm_op shr 16) and &hf 
    dim as BOOL   w   = (arm_op shr 21) and &h1 
    dim as BOOL   u   = (arm_op shr 23) and &h1 
    dim as BOOL   p   = (arm_op shr 24) and &h1 

    dim as arm_memio_t op
			 op.rt=rt
			 op.addr=arm_r.r(rn)

    if (rn = 15) then op.addr and= INV(3) 

    dim as long disp 

    if (u) then 
        disp =  imm 
    else
        disp = -imm
    endif
  
    if (p) then op.addr+= disp 
    if (p=0) or (w<>0) then arm_r.r(rn) += disp 

    return op 
end function

function arm_memio_reg_op() as arm_memio_t 
If DEB=3 Then Print "arm_memio_reg_op"
    dim as ubyte rm   = (arm_op shr  0) and &hf 
    dim as ubyte tipo = (arm_op shr  5) and &h3 
    dim as ubyte imm  = (arm_op shr  7) and &h1f 
    dim as ubyte rt   = (arm_op shr 12) and &hf 
    dim as ubyte rn   = (arm_op shr 16) and &hf 
    dim as BOOL  w    = (arm_op shr 21) and &h1 
    dim as BOOL  u    = (arm_op shr 23) and &h1 
    dim as BOOL  p    = (arm_op shr 24) and &h1 

    dim as arm_memio_t op
			 op.rt=rt
			 op.addr=arm_r.r(rn)

    if (rn = 15) then op.addr and= INV(3)

    dim as arm_shifter_t shift = arm_data_regi(rm, tipo, imm) 

    dim as long disp 

    if (u) then 
        disp =  shift.valor 
    else
        disp = -shift.valor
    endif

    if (p) then op.addr+= disp 
    if (p=0) or (w<>0) then arm_r.r(rn) += disp 

    return op 
end function

function arm_memio_immt_op() as arm_memio_t 
If DEB=3 Then Print "arm_memio_immt_op"
    dim as ulong imm  = (arm_op shr  0) and &hfff 
    dim as ubyte  rt  = (arm_op shr 12) and &hf 
    dim as ubyte  rn  = (arm_op shr 16) and &hf 
    dim as BOOL   u   = (arm_op shr 23) and &h1 

    dim as arm_memio_t op
			 op.rt=rt
			 op.addr=arm_r.r(rn)

    if (u) then 
        arm_r.r(rn) += imm 
    else
        arm_r.r(rn) -= imm
    endif

    return op 
end function

function arm_memio_regt_op() as arm_memio_t 
If DEB=3 Then Print "arm_memio_regt_op"
    dim as ubyte rm   = (arm_op shr  0) and &hf 
    dim as ubyte tipo = (arm_op shr  5) and &h3 
    dim as ubyte imm  = (arm_op shr  7) and &h1f 
    dim as ubyte rt   = (arm_op shr 12) and &hf 
    dim as ubyte rn   = (arm_op shr 16) and &hf 
    dim as BOOL  u    = (arm_op shr 23) and &h1 

    dim as arm_memio_t op
			 op.rt=rt
			 op.addr=arm_r.r(rn)

    dim as arm_shifter_t shift = arm_data_regi(rm, tipo, imm) 

    if (u) then 
        arm_r.r(rn) += shift.valor 
    else
        arm_r.r(rn) -= shift.valor
    endif
  
    return op 
end function

function arm_memio_immdh_op() as arm_memio_t 
If DEB=3 Then Print "arm_memio_immdh_op"
    dim as ubyte  rt  = (arm_op shr 12) and &hf 
    dim as ubyte  rn  = (arm_op shr 16) and &hf 
    dim as BOOL   w   = (arm_op shr 21) and &h1 
    dim as BOOL   u   = (arm_op shr 23) and &h1 
    dim as BOOL   p   = (arm_op shr 24) and &h1 

    dim as arm_memio_t op
			 op.rt=rt
			 op.rt2=rt or 1
			 op.addr=arm_r.r(rn)

    if (rn = 15) then op.addr and=INV(3) 

    dim as ushort imm 
    dim as long disp 

    imm  =  (arm_op shr 0) and &hf 
    imm or= (arm_op shr 4) and &hf0 

    if (u) then 
        disp =  imm 
    else
        disp = -imm
    endif
  
    if (p) then op.addr += disp 
    if (p=0) or (w<>0) then arm_r.r(rn) += disp 

    return op 
end function

function arm_memio_regdh_op() as arm_memio_t 
If DEB=3 Then Print "arm_memio_regdh_op"
    dim as ubyte rm = (arm_op shr  0) and &hf 
    dim as ubyte rt = (arm_op shr 12) and &hf 
    dim as ubyte rn = (arm_op shr 16) and &hf 
    dim as BOOL  w  = (arm_op shr 21) and &h1 
    dim as BOOL  u  = (arm_op shr 23) and &h1 
    dim as BOOL  p  = (arm_op shr 24) and &h1 

    dim as arm_memio_t op
			 op.rt=rt
			 op.rt2=rt or 1
			 op.addr=arm_r.r(rn)

    if (rn = 15) then op.addr and=  INV(3) 

    dim as long disp 

    if (u) then 
        disp =  arm_r.r(rm) 
    else
        disp = -arm_r.r(rm)
    endif
  
    if (p) then op.addr += disp 
    if (p=0) or (w<>0) then arm_r.r(rn) += disp 

    return op 
end function

function t16_memio_mult_op() as arm_memio_t 
If DEB=3 Then Print "t16_memio_mult_op"
    dim as ubyte regs = (arm_op shr 0) and &hff 
    dim as ubyte rn   = (arm_op shr 8) and &h7 

    dim as ubyte i, cnt = 0 

    for i = 0 to 7        
        if (regs and (1 shl i)) then cnt += 4 
    next 

    dim as arm_memio_t op
			 op.regs=regs
			 op.rn=rn
			 op.addr=arm_r.r(rn) and INV(3)
			 op.disp=cnt

    return op 
end function

function t16_memio_imm5_op(stepp as byte) as arm_memio_t 
If DEB=3 Then Print "t16_memio_imm5_op"
    dim as ubyte rt  = (arm_op shr 0) and &h7 
    dim as ubyte imm = (arm_op shr 6) and &h1f 
    dim as ubyte rn  = (arm_op shr 3) and &h7 

    dim as ulong addr = arm_r.r(rn) + imm * stepp 

    dim as arm_memio_t op 
			 op.rt=rt
			 op.addr=addr

    return op 
end function

function t16_memio_imm8sp_op() as arm_memio_t 
If DEB=3 Then Print "t16_memio_imm8sp_op"
    dim as ushort imm = (arm_op shl 2) and &h3fc 
    dim as ubyte  rt  = (arm_op shr 8) and &h7 

    dim as ulong addr = arm_r.r(13) + imm 

    dim as arm_memio_t op
			 op.rt=rt
			 op.addr=addr

    return op 
end function

function t16_memio_imm8pc_op() as arm_memio_t 
If DEB=3 Then Print "t16_memio_imm8pc_op"
    dim as ushort imm = (arm_op shl 2) and &h3fc 
    dim as ubyte  rt  = (arm_op shr 8) and &h7 

    dim as ulong addr = (arm_r.r(15) and  INV(3)) + imm 

    dim as arm_memio_t op
			 op.rt=rt
			 op.addr=addr

    return op 
end function

function t16_memio_reg_op() as arm_memio_t 
If DEB=3 Then Print "t16_memio_reg_op"
    dim as ubyte rt = (arm_op shr 0) and &h7 
    dim as ubyte rn = (arm_op shr 3) and &h7 
    dim as ubyte rm = (arm_op shr 6) and &h7 

    dim as ulong addr = arm_r.r(rn) + arm_r.r(rm) 

    dim as arm_memio_t op
			 op.rt=rt
			 op.addr=addr

    return op 
end function


' Parallel arithmetic
sub arm_parith_qadd(op as arm_parith_t) 
If DEB=3 Then Print "arm_parith_qadd"
    dim as longint lhs = op.lhs.w 
    dim as LongInt rhs = op.rhs.w 

    arm_r.r(op.rd) = arm_ssatq(lhs + rhs) 
end sub

sub arm_parith_qsub(op as arm_parith_t) 
If DEB=3 Then Print "arm_parith_qsub"
    dim as LongInt lhs = op.lhs.w 
    dim as LongInt rhs = op.rhs.w 

    arm_r.r(op.rd) = arm_ssatq(lhs - rhs) 
end sub

sub arm_parith_qdadd(op as arm_parith_t) 
If DEB=3 Then Print "arm_parith_qdadd"
    dim as LongInt lhs = op.lhs.w 
    dim as LongInt rhs = op.rhs.w 

    dim as long doubled = arm_ssatq(rhs shl 1) 

    arm_r.r(op.rd) = arm_ssatq(lhs + doubled) 
end sub

sub arm_parith_qdsub(op as arm_parith_t) 
If DEB=3 Then Print "arm_parith_qdsub"
    dim as LongInt lhs = op.lhs.w 
    dim as LongInt rhs = op.rhs.w 

    dim as long doubled = arm_ssatq(rhs shl 1) 

    arm_r.r(op.rd) = arm_ssatq(lhs - doubled) 
end sub

function arm_parith_op() as arm_parith_t 
If DEB=3 Then Print "arm_parith_op"
    dim as ubyte rm = (arm_op shr  0) and &hf 
    dim as ubyte rd = (arm_op shr 12) and &hf 
    dim as ubyte rn = (arm_op shr 16) and &hf 

    dim as arm_parith_t op 
    		op.lhs.w=arm_r.r(rm)
    		op.rhs.w=arm_r.r(rn)
    		op.rd =rd

    return op 
end function

'add with carry
sub arm_adc_imm() 
If DEB=3 Then Print "arm_adc_imm"
    arm_arith_add(arm_data_imm_op(), _ARM_ARITH_CARRY) 
end sub

sub arm_adc_regi() 
If DEB=3 Then Print "arm_adc_regi"
    arm_arith_add(arm_data_regi_op(), _ARM_ARITH_CARRY) 
end sub

sub arm_adc_regr() 
If DEB=3 Then Print "arm_adc_regr"
    arm_arith_add(arm_data_regr_op(), _ARM_ARITH_CARRY) 
end sub

sub t16_adc_rdn3() 
If DEB=3 Then Print "t16_adc_rdn3"
    arm_arith_add(t16_data_rdn3_op(), _ARM_ARITH_CARRY) 
end sub

'add
sub arm_add_imm() 
If DEB=3 Then Print "arm_add_imm"
    arm_arith_add(arm_data_imm_op(), _ARM_ARITH_NO_C) 
end sub

sub arm_add_regi() 
If DEB=3 Then Print "arm_add_regi"
    arm_arith_add(arm_data_regi_op(), _ARM_ARITH_NO_C) 
end sub

sub arm_add_regr() 
If DEB=3 Then Print "arm_add_regr"
    arm_arith_add(arm_data_regr_op(), _ARM_ARITH_NO_C) 
end sub

sub t16_add_imm3() 
If DEB=3 Then Print "t16_add_imm3"
    arm_arith_add(t16_data_imm3_op(), _ARM_ARITH_NO_C) 
end sub

sub t16_add_imm8() 
If DEB=3 Then Print "t16_add_imm8"
    arm_arith_add(t16_data_imm8_op(), _ARM_ARITH_NO_C) 
end sub

sub t16_add_reg() 
If DEB=3 Then Print "t16_add_reg"
    arm_arith_add(t16_data_reg_op(), _ARM_ARITH_NO_C) 
end sub

sub t16_add_rdn4() 
If DEB=3 Then Print "t16_add_rdn4"
    arm_arith_add(t16_data_rdn4_op(_ARM_FLAG_KEEP), _ARM_ARITH_NO_C) 
end sub

sub t16_add_sp7() 
If DEB=3 Then Print "t16_add_sp7"
    arm_arith_add(t16_data_imm7sp_op(), _ARM_ARITH_NO_C) 
end sub

sub t16_add_sp8() 
If DEB=3 Then Print "t16_add_sp8"
    arm_arith_add(t16_data_imm8sp_op(), _ARM_ARITH_NO_C) 
end sub

sub t16_adr() 
If DEB=3 Then Print "t16_adr"
    arm_arith_add(t16_data_imm8pc_op(), _ARM_ARITH_NO_C) 
end sub

'and
sub arm_and_imm() 
If DEB=3 Then Print "arm_and_imm"
    arm_logic(arm_data_imm_op(), ARD) 
end sub

sub arm_and_regi() 
If DEB=3 Then Print "arm_and_regi"
    arm_logic(arm_data_regi_op(), ARD) 
end sub

sub arm_and_regr() 
If DEB=3 Then Print "arm_and_regr"
    arm_logic(arm_data_regr_op(), ARD) 
end sub

sub t16_and_rdn3() 
If DEB=3 Then Print "t16_and_rdn3"
    arm_logic(t16_data_rdn3_op(), ARD) 
end sub

'bit shift (asr, lsl, ...)
sub arm_shift_imm() 
If DEB=3 Then Print "arm_shift_imm"
    arm_logic(arm_data_regi_op(), SHF) 
end sub

sub arm_shift_reg() 
If DEB=3 Then Print "arm_shift_reg"
    arm_logic(arm_data_regr_op(), SHF) 
end sub

'arithmetic shift right
sub t16_asr_imm5() 
If DEB=3 Then Print "t16_asr_imm5"
    arm_asr(t16_data_imm5_op(_ARM_SHIFT_RIGHT)) 
end sub

sub t16_asr_rdn3() 
If DEB=3 Then Print "t16_asr_rdn3"
    arm_asr(t16_data_rdn3_op()) 
end sub

'branch
sub arm_b() 
If DEB=3 Then Print "arm_b"
    dim as long imm = arm_op 

    imm shl = 8 
    imm shr = 6 

    arm_r.r(15) += imm 

    arm_load_pipe() 
end sub

sub t16_b_imm11() 
If DEB=3 Then Print "t16_b_imm11"
    dim as long imm = arm_op 

    imm shl = 21 
    imm shr = 20 

    arm_r.r(15) += imm 

    arm_load_pipe() 
end sub

'thumb conditional branches
sub t16_b_imm8() 
If DEB=3 Then Print "t16_b_imm8"
    dim as long imm = arm_op 

    imm shl = 24 
    imm shr = 23 

    dim as byte cond = (arm_op shr 8) and &hf 

    if (arm_cond(cond)) then 
        arm_r.r(15) += imm 
        arm_load_pipe() 
    endif
  
end sub

'bit clear
sub arm_bic_imm() 
If DEB=3 Then Print "arm_bic_imm"
    arm_logic(arm_data_imm_op(), BIC) 
end sub

sub arm_bic_regi() 
If DEB=3 Then Print "arm_bic_regi"
    arm_logic(arm_data_regi_op(), BIC) 
end sub

sub arm_bic_regr() 
If DEB=3 Then Print "arm_bic_regr"
    arm_logic(arm_data_regr_op(), BIC) 
end sub

sub t16_bic_rdn3() 
If DEB=3 Then Print "t16_bic_rdn3"
    arm_logic(t16_data_rdn3_op(), BIC) 
end sub

'breakpoint
sub arm_bkpt() 
If DEB=3 Then Print "arm_bkpt"
    arm_int(_ARM_VEC_PABT, _ARM_ABT) 
end sub

sub t16_bkpt() 
If DEB=3 Then Print "t16_bkpt"
    arm_int(_ARM_VEC_PABT, _ARM_ABT) 
end sub

'branch with link
sub arm_bl() 
If DEB=3 Then Print "arm_bl"
    dim as long imm = arm_op 

    imm shl = 8 
    imm shr = 6 

    arm_r.r(14) =  arm_r.r(15) - _ARM_WORD_SZ 
    arm_r.r(15) = (arm_r.r(15) And INV(3)) + imm 

    arm_load_pipe() 
end sub

'branch with link and exchange
sub arm_blx_imm() 
If DEB=3 Then Print "arm_blx_imm"
    dim as long imm = arm_op 

    imm shl = 8 
    imm shr = 6 

    imm or= (arm_op shr 23) and 2 

    arm_flag_set(_ARM_T , TRUE) 

    arm_r.r(14)  = arm_r.r(15) - _ARM_WORD_SZ 
    arm_r.r(15) += imm 

    arm_load_pipe() 
end sub

sub arm_blx_reg() 
If DEB=3 Then Print "arm_blx_reg"
    dim as ubyte rm = arm_op and &hf 

    arm_r.r(14) = arm_r.r(15) - _ARM_WORD_SZ 
    arm_r.r(15) = arm_r.r(rm) 

    arm_interwork() 
end sub

sub t16_blx() 
If DEB=3 Then Print "t16_blx"
    dim as ubyte rm = (arm_op shr 3) and &hf 

    arm_r.r(14) = (arm_r.r(15) - _ARM_HWORD_SZ) or 1 
    arm_r.r(15) =  arm_r.r(rm) 

    arm_interwork() 
end sub

sub t16_blx_h2() 
If DEB=3 Then Print "t16_blx_h2"
    dim as long imm = arm_op 

    imm shl = 21 
    imm shr = 9 

    arm_r.r(14)  = arm_r.r(15) 
    arm_r.r(14) += imm 
end sub

sub t16_blx_lrimm() 
If DEB=3 Then Print "t16_blx_lrimm"
    dim as ulong imm = arm_r.r(14) 

    imm += (arm_op and &h7ff) shl 1 

    arm_r.r(14) = (arm_r.r(15) - _ARM_HWORD_SZ) or 1 
    arm_r.r(15) = imm and  INV(1) 
end sub

sub t16_blx_h1() 
If DEB=3 Then Print "t16_blx_h1"
    t16_blx_lrimm() 
    arm_interwork() 
end sub

sub t16_blx_h3() 
If DEB=3 Then Print "t16_blx_h3"
    t16_blx_lrimm() 
    arm_load_pipe() 
end sub

'branch and exchange
sub arm_bx() 
If DEB=3 Then Print "arm_bx"
    dim as ubyte rm = arm_op and &hf 

    arm_r.r(15) = arm_r.r(rm) 

    arm_interwork() 
end sub

sub t16_bx() 
If DEB=3 Then Print "t16_bx"
    dim as ubyte rm = (arm_op shr 3) and &hf 

    arm_r.r(15) = arm_r.r(rm) 

    arm_interwork() 
end sub

'coprocessor data processing
sub arm_cdp() 
If DEB=3 Then Print "arm_cdp"
    'no coprocessors used
end sub

sub arm_cdp2() 
If DEB=3 Then Print "arm_cdp2"
    'no coprocessors used
end sub

'count leading zeros
sub arm_clz() 
If DEB=3 Then Print "arm_clz"
    arm_count_zeros((arm_op shr 12) and &hf) 
end sub

'compare negative
sub arm_cmn_imm() 
If DEB=3 Then Print "arm_cmn_imm"
    arm_arith_cmn(arm_data_imm_op()) 
end sub

sub arm_cmn_regi() 
If DEB=3 Then Print "arm_cmn_regi"
    arm_arith_cmn(arm_data_regi_op()) 
end sub

sub arm_cmn_regr() 
If DEB=3 Then Print "arm_cmn_regr"
    arm_arith_cmn(arm_data_regr_op()) 
end sub

sub t16_cmn_rdn3() 
If DEB=3 Then Print "t16_cmn_rdn3"
    arm_arith_cmn(t16_data_rdn3_op()) 
end sub

'compare
sub arm_cmp_imm() 
If DEB=3 Then Print "arm_cmp_imm"
    arm_arith_cmp(arm_data_imm_op()) 
end sub

sub arm_cmp_regi() 
If DEB=3 Then Print "arm_cmp_regi"
    arm_arith_cmp(arm_data_regi_op()) 
end sub

sub arm_cmp_regr() 
If DEB=3 Then Print "arm_cmp_regr"
    arm_arith_cmp(arm_data_regr_op()) 
end sub

sub t16_cmp_imm8() 
If DEB=3 Then Print "t16_cmp_imm8"
    arm_arith_cmp(t16_data_imm8_op()) 
end sub

sub t16_cmp_rdn3() 
If DEB=3 Then Print "t16_cmp_rdn3"
    arm_arith_cmp(t16_data_rdn3_op()) 
end sub

sub t16_cmp_rdn4() 
If DEB=3 Then Print "t16_cmp_rdn4"
    arm_arith_cmp(t16_data_rdn4_op(TRUE)) 
end sub

'exclusive or
sub arm_eor_imm() 
If DEB=3 Then Print "arm_eor_imm"
    arm_logic(arm_data_imm_op(), eor) 
end sub

sub arm_eor_regi() 
If DEB=3 Then Print "arm_eor_regi"
    arm_logic(arm_data_regi_op(), eor) 
end sub

sub arm_eor_regr() 
If DEB=3 Then Print "arm_eor_regr"
    arm_logic(arm_data_regr_op(), eor) 
end sub

sub t16_eor_rdn3() 
If DEB=3 Then Print "t16_eor_rdn3"
    arm_logic(t16_data_rdn3_op(), eor) 
end sub

'load coprocessor
sub arm_ldc() 
If DEB=3 Then Print "arm_ldc"
    'no coprocessors used
end sub

sub arm_ldc2() 
If DEB=3 Then Print "arm_ldc2"
    'no coprocessors used
end sub

'load multiple
sub arm_ldm() 
If DEB=3 Then Print "arm_ldm"
    arm_memio_ldm(arm_memio_mult_op()) 
end sub

sub arm_ldm_usr() 
If DEB=3 Then Print "arm_ldm_usr"
    arm_memio_ldm_usr(arm_memio_mult_op()) 
end sub

sub t16_ldm() 
If DEB=3 Then Print "t16_ldm"
    arm_memio_ldm(t16_memio_mult_op()) 
end sub

'load register
sub arm_ldr_imm() 
If DEB=3 Then Print "arm_ldr_imm"
    arm_memio_ldr(arm_memio_imm_op()) 
end sub

sub arm_ldr_reg() 
If DEB=3 Then Print "arm_ldr_reg"
    arm_memio_ldr(arm_memio_reg_op()) 
end sub

sub t16_ldr_imm5() 
If DEB=3 Then Print "t16_ldr_imm5"
    arm_memio_ldr(t16_memio_imm5_op(_ARM_WORD_SZ)) 
end sub

sub t16_ldr_sp8() 
If DEB=3 Then Print "t16_ldr_sp8"
    arm_memio_ldr(t16_memio_imm8sp_op()) 
end sub

sub t16_ldr_pc8() 
If DEB=3 Then Print "t16_ldr_pc8"
    arm_memio_ldr(t16_memio_imm8pc_op()) 
end sub

sub t16_ldr_reg() 
If DEB=3 Then Print "t16_ldr_reg"
    arm_memio_ldr(t16_memio_reg_op()) 
end sub

'load register byte
sub arm_ldrb_imm() 
If DEB=3 Then Print "arm_ldrb_imm"
    arm_memio_ldrb(arm_memio_imm_op()) 
end sub

sub arm_ldrb_reg() 
If DEB=3 Then Print "arm_ldrb_reg"
    arm_memio_ldrb(arm_memio_reg_op()) 
end sub

sub t16_ldrb_imm5() 
If DEB=3 Then Print "t16_ldrb_imm5"
    arm_memio_ldrb(t16_memio_imm5_op(_ARM_BYTE_SZ)) 
end sub

sub t16_ldrb_reg() 
If DEB=3 Then Print "t16_ldrb_reg"
    arm_memio_ldrb(t16_memio_reg_op()) 
end sub

'load register byte unprivileged
sub arm_ldrbt_imm() 
If DEB=3 Then Print "arm_ldrbt_imm"
    arm_memio_ldrbt(arm_memio_immt_op()) 
end sub

sub arm_ldrbt_reg() 
If DEB=3 Then Print "arm_ldrbt_reg"
    arm_memio_ldrbt(arm_memio_regt_op()) 
end sub

'load register dual
sub arm_ldrd_imm() 
If DEB=3 Then Print "arm_ldrd_imm"
    arm_memio_ldrd(arm_memio_immdh_op()) 
end sub

sub arm_ldrd_reg() 
If DEB=3 Then Print "arm_ldrd_reg"
    arm_memio_ldrd(arm_memio_regdh_op()) 
end sub

'load register halfword
sub arm_ldrh_imm() 
If DEB=3 Then Print "arm_ldrh_imm"
    arm_memio_ldrh(arm_memio_immdh_op()) 
end sub

sub arm_ldrh_reg() 
If DEB=3 Then Print "arm_ldrh_reg"
    arm_memio_ldrh(arm_memio_regdh_op()) 
end sub

sub t16_ldrh_imm5() 
If DEB=3 Then Print "t16_ldrh_imm5"
    arm_memio_ldrh(t16_memio_imm5_op(_ARM_HWORD_SZ)) 
end sub

sub t16_ldrh_reg() 
If DEB=3 Then Print "t16_ldrh_reg"
    arm_memio_ldrh(t16_memio_reg_op()) 
end sub

'load register signed byte
sub arm_ldrsb_imm() 
If DEB=3 Then Print "arm_ldrsb_imm"
    arm_memio_ldrsb(arm_memio_immdh_op()) 
end sub

sub arm_ldrsb_reg() 
If DEB=3 Then Print "arm_ldrsb_reg"
    arm_memio_ldrsb(arm_memio_regdh_op()) 
end sub

sub t16_ldrsb_reg() 
If DEB=3 Then Print "t16_ldrsb_reg"
    arm_memio_ldrsb(t16_memio_reg_op()) 
end sub

'load register signed halfword
sub arm_ldrsh_imm() 
If DEB=3 Then Print "arm_ldrsh_imm"
    arm_memio_ldrsh(arm_memio_immdh_op()) 
end sub

sub arm_ldrsh_reg() 
If DEB=3 Then Print "arm_ldrsh_reg"
    arm_memio_ldrsh(arm_memio_regdh_op()) 
end sub

sub t16_ldrsh_reg() 
If DEB=3 Then Print "t16_ldrsh_reg"
    arm_memio_ldrsh(t16_memio_reg_op()) 
end sub

'logical shift left
sub t16_lsl_imm5() 
If DEB=3 Then Print "t16_lsl_imm5"
    arm_lsl(t16_data_imm5_op(_ARM_SHIFT_LEFT)) 
end sub

sub t16_lsl_rdn3() 
If DEB=3 Then Print "t16_lsl_rdn3"
    arm_lsl(t16_data_rdn3_op()) 
end sub

'logical shift right
sub t16_lsr_imm5() 
If DEB=3 Then Print "t16_lsr_imm5"
    arm_lsr(t16_data_imm5_op(_ARM_SHIFT_RIGHT)) 
end sub

sub t16_lsr_rdn3() 
If DEB=3 Then Print "t16_lsr_rdn3"
    arm_lsr(t16_data_rdn3_op()) 
end sub

'move to coprocessor from register
sub arm_mcr() 
If DEB=3 Then Print "arm_mcr"
    'no coprocessors used
end sub

sub arm_mcr2() 
If DEB=3 Then Print "arm_mcr2"
    'no coprocessors used
end sub

'move to coprocessor from two registers
sub arm_mcrr() 
If DEB=3 Then Print "arm_mcrr"
    'no coprocessors used
end sub

sub arm_mcrr2() 
If DEB=3 Then Print "arm_mcrr2"
    'no coprocessors used
end sub

'multiply add
sub arm_mla() 
If DEB=3 Then Print "arm_mla"
    arm_mpy_add(arm_mpy_op()) 
end sub

'move
sub arm_mov_imm12() 
If DEB=3 Then Print "arm_mov_imm12"
    dim as arm_data_t op = arm_data_imm_op() 

    arm_r.r(op.rd) = op.rhs 

    arm_logic_set(op, op.rhs) 
end sub

sub t16_mov_imm() 
If DEB=3 Then Print "t16_mov_imm"
    dim as ubyte imm = (arm_op shr 0) and &hff 
    dim as ubyte rd  = (arm_op shr 8) and &h7 

    arm_r.r(rd) = imm 

    arm_setn(arm_r.r(rd)) 
    arm_setz(arm_r.r(rd)) 
end sub

sub t16_mov_rd4() 
If DEB=3 Then Print "t16_mov_rd4"
    dim as ubyte rm = (arm_op shr 3) and &hf 

    dim as ubyte rd 

    rd   = (arm_op shr 0) and 7 
    rd or= (arm_op shr 4) and 8 

    arm_r.r(rd) = arm_r.r(rm) 

    if (rd = 15) then 
        arm_r.r(rd) and=  INV(1) 
        arm_load_pipe() 
    endif
  
end sub

sub t16_mov_rd3() 
If DEB=3 Then Print "t16_mov_rd3"
    dim as byte rd = (arm_op shr 0) and 7 
    dim as byte rm = (arm_op shr 3) and 7 

    arm_r.r(rd) = arm_r.r(rm) 

    arm_setn(arm_r.r(rd)) 
    arm_setz(arm_r.r(rd)) 
end sub

'move to register from coprocessor
sub arm_mrc() 
If DEB=3 Then Print "arm_mrc"
    'no coprocessors used
end sub

sub arm_mrc2() 
If DEB=3 Then Print "arm_mrc2"
    'no coprocessors used
end sub

'move to two registers from coprocessor
sub arm_mrrc() 
If DEB=3 Then Print "arm_mrrc"
    'no coprocessors used
end sub

sub arm_mrrc2() 
If DEB=3 Then Print "arm_mrrc2"
    'no coprocessors used
end sub

'move to register from status
sub arm_mrs() 
If DEB=3 Then Print "arm_mrs"
    arm_psr_to_reg(arm_mrs_op()) 
end sub

'move to status from register (also nop/sev)
sub arm_msr_imm() 
If DEB=3 Then Print "arm_msr_imm"
    arm_reg_to_psr(arm_msr_imm_op()) 
end sub

sub arm_msr_reg() 
If DEB=3 Then Print "arm_msr_reg"
    arm_reg_to_psr(arm_msr_reg_op()) 
end sub

'multiply
sub arm_mul() 
If DEB=3 Then Print "arm_mul"
    arm_mpy(arm_mpy_op()) 
end sub

sub t16_mul() 
If DEB=3 Then Print "t16_mul"
    arm_mpy(t16_mpy_op()) 
end sub

'move not
sub arm_mvn_imm() 
If DEB=3 Then Print "arm_mvn_imm"
    arm_logic(arm_data_imm_op(), MVN) 
end sub

sub arm_mvn_regi() 
If DEB=3 Then Print "arm_mvn_regi"
    arm_logic(arm_data_regi_op(), MVN) 
end sub

sub arm_mvn_regr() 
If DEB=3 Then Print "arm_mvn_regr"
    arm_logic(arm_data_regr_op(), MVN) 
end sub

sub t16_mvn_rdn3() 
If DEB=3 Then Print "t16_mvn_rdn3"
    arm_logic(t16_data_rdn3_op(), MVN) 
end sub

'or
sub arm_orr_imm() 
If DEB=3 Then Print "arm_orr_imm"
    arm_logic(arm_data_imm_op(), ORR) 
end sub

sub arm_orr_regi() 
If DEB=3 Then Print "arm_orr_regi"
    arm_logic(arm_data_regi_op(), ORR) 
end sub

sub arm_orr_regr() 
If DEB=3 Then Print "arm_orr_regr"
    arm_logic(arm_data_regr_op(), ORR) 
end sub

sub t16_orr_rdn3() 
If DEB=3 Then Print "t16_orr_rdn3"
    arm_logic(t16_data_rdn3_op(), ORR) 
end sub

'preload data
sub arm_pld_imm() 
If DEB=3 Then Print "arm_pld_imm"
    'performance oriented instruction, just ignore
end sub

sub arm_pld_reg() 
If DEB=3 Then Print "arm_pld_reg"
    'performance oriented instruction, just ignore
end sub

'pop
sub t16_pop() 
If DEB=3 Then Print "t16_pop"
    dim as ushort regs 

    regs   = (arm_op shr 0) and &h00ff 
    regs or= (arm_op shl 7) and &h8000 

    dim as ubyte i 
    
    ' NOTA IMPORTANTE: segun la documentacion, SP (reg(13)) deberia alinearse a 4 bytes (bits 0 y 1)
    ' por eso aqui se hace un AND-3 (filtrar bits 0 y 1 para alinear a 4)
    ' lo que ocurre es que, el TEST de THUMB 225 de alineamiento PUSH-POP da error
    ' en cambio, si quito lo de alinear a 4, funciona, pero..... es correcto?
    'arm_r.r(13) and= INV(3) 

    for i = 0 to 15         
        if (regs and (1 shl i)) then 
            arm_r.r(i) = arm_read_s(arm_r.r(13)) 
            arm_r.r(13) += 4 
        endif
    next

    if (regs and &h8000) then 
        arm_r.r(15) and= INV(1) 
        arm_load_pipe() 
    endif
  
end sub

'push
sub t16_push() 
If DEB=3 Then Print "t16_push"
    dim as ushort regs 

    regs  = (arm_op shr 0) and &h00ff 
    regs or= (arm_op shl 6) and &h4000 

    ' NOTA IMPORTANTE: segun la documentacion, SP (reg(13)) deberia alinearse a 4 bytes (bits 0 y 1)
    ' por eso aqui se hace un AND-3 (filtrar bits 0 y 1 para alinear a 4)
    ' lo que ocurre es que, el TEST de THUMB 225 de alineamiento PUSH-POP da error
    ' en cambio, si quito lo de alinear a 4, funciona, pero..... es correcto?
    dim as ulong addr = arm_r.r(13)' and INV(3) 

    dim as ubyte i 

    for i = 0 to 15         
        if (regs and (1 shl i)) then addr -= 4 
    next

    arm_r.r(13) = addr 

    for i = 0 to 15         
        if (regs and (1 shl i)) then 
            arm_write_s(addr, arm_memio_reg_get(i)) 
            addr += 4 
        endif
    next

end sub

'saturating add
sub arm_qadd() 
If DEB=3 Then Print "arm_qadd"
    arm_parith_qadd(arm_parith_op()) 
end sub

'saturating double and add
sub arm_qdadd() 
If DEB=3 Then Print "arm_qdadd"
    arm_parith_qdadd(arm_parith_op()) 
end sub

'saturating double and subtract
sub arm_qdsub() 
If DEB=3 Then Print "arm_qdsub"
    arm_parith_qdsub(arm_parith_op()) 
end sub

'saturating subtract
sub arm_qsub() 
If DEB=3 Then Print "arm_qsub"
    arm_parith_qsub(arm_parith_op()) 
end sub

'rotate right
sub t16_ror() 
If DEB=3 Then Print "t16_ror"
    arm_ror(t16_data_rdn3_op()) 
end sub

'reverse subtract
sub arm_rsb_imm() 
If DEB=3 Then Print "arm_rsb_imm"
    arm_arith_rsb(arm_data_imm_op()) 
end sub

sub arm_rsb_regi() 
If DEB=3 Then Print "arm_rsb_regi"
    arm_arith_rsb(arm_data_regi_op()) 
end sub

sub arm_rsb_regr() 
If DEB=3 Then Print "arm_rsb_regr"
    arm_arith_rsb(arm_data_regr_op()) 
end sub

sub t16_rsb_rdn3() 
If DEB=3 Then Print "t16_rsb_rdn3"
    arm_arith_sub(t16_data_neg_op()) 
end sub

'reverse subtract with carry
sub arm_rsc_imm() 
If DEB=3 Then Print "arm_rsc_imm"
    arm_arith_rsc(arm_data_imm_op()) 
end sub

sub arm_rsc_regi() 
If DEB=3 Then Print "arm_rsc_regi"
    arm_arith_rsc(arm_data_regi_op()) 
end sub

sub arm_rsc_regr() 
If DEB=3 Then Print "arm_rsc_regr"
    arm_arith_rsc(arm_data_regr_op()) 
end sub

'subtract with carry
sub arm_sbc_imm() 
If DEB=3 Then Print "arm_sbc_imm"
    arm_arith_sbc(arm_data_imm_op()) 
end sub

sub arm_sbc_regi() 
If DEB=3 Then Print "arm_sbc_regi"
    arm_arith_sbc(arm_data_regi_op()) 
end sub

sub arm_sbc_regr() 
If DEB=3 Then Print "arm_sbc_regr"
    arm_arith_sbc(arm_data_regr_op()) 
end sub

sub t16_sbc_rdn3() 
If DEB=3 Then Print "t16_sbc_rdn3"
    arm_arith_sbc(t16_data_rdn3_op()) 
end sub

'signed multiply accumulate
sub arm_smla__() 
If DEB=3 Then Print "arm_smla__"
    dim as BOOL m = (arm_op shr 5) and 1 
    dim as BOOL n = (arm_op shr 6) and 1 

    arm_mpy_smla__(arm_mpy_op(), m, n) 
end sub

'signed multiply accumulate long
sub arm_smlal() 
If DEB=3 Then Print "arm_smlal"
    arm_mpy_smlal(arm_mpy_op()) 
end sub

'signed multiply accumulate long (halfwords)
sub arm_smlal__() 
If DEB=3 Then Print "arm_smlal__"
    dim as BOOL m = (arm_op shr 5) and 1 
    dim as BOOL n = (arm_op shr 6) and 1 

    arm_mpy_smlal__(arm_mpy_op(), m, n) 
end sub

'signed multiply accumulate word by halfword
sub arm_smlaw_() 
If DEB=3 Then Print "arm_smlaw_"
    arm_mpy_smlaw_(arm_mpy_op(), (arm_op shr 6) and 1) 
end sub

'signed multiply
sub arm_smul() 
If DEB=3 Then Print "arm_smul"
    dim as BOOL m = (arm_op shr 5) and 1 
    dim as BOOL n = (arm_op shr 6) and 1 

    arm_mpy_smul(arm_mpy_op(), m, n) 
end sub

'signed multiply long
sub arm_smull() 
If DEB=3 Then Print "arm_smull"
    arm_mpy_smull(arm_mpy_op()) 
end sub

'signed multiply word by halfword
sub arm_smulw_() 
If DEB=3 Then Print "arm_smulw_"
    arm_mpy_smulw_(arm_mpy_op(), (arm_op shr 6) and 1) 
end sub

'store coprocessor
sub arm_stc() 
If DEB=3 Then Print "arm_stc"
    'no coprocessors used
end sub

sub arm_stc2() 
If DEB=3 Then Print "arm_stc2"
    'no coprocessors used
end sub

'store multiple
sub arm_stm() 
If DEB=3 Then Print "arm_stm"
    arm_memio_stm(arm_memio_mult_op()) 
end sub

sub arm_stm_usr() 
If DEB=3 Then Print "arm_stm_usr"
    arm_memio_stm_usr(arm_memio_mult_op()) 
end sub

sub t16_stm() 
If DEB=3 Then Print "t16_stm"
    arm_memio_stm(t16_memio_mult_op()) 
end sub

'store register
sub arm_str_imm() 
If DEB=3 Then Print "arm_str_imm"
    arm_memio_str(arm_memio_imm_op()) 
end sub

sub arm_str_reg() 
If DEB=3 Then Print "arm_str_reg"
    arm_memio_str(arm_memio_reg_op()) 
end sub

sub t16_str_imm5() 
If DEB=3 Then Print "t16_str_imm5"
    arm_memio_str(t16_memio_imm5_op(_ARM_WORD_SZ)) 
end sub

sub t16_str_sp8() 
If DEB=3 Then Print "t16_str_sp8"
    arm_memio_str(t16_memio_imm8sp_op()) 
end sub

sub t16_str_reg() 
If DEB=3 Then Print "t16_str_reg"
    arm_memio_str(t16_memio_reg_op()) 
end sub

'store register byte
sub arm_strb_imm() 
If DEB=3 Then Print "arm_strb_imm"
    arm_memio_strb(arm_memio_imm_op()) 
end sub

sub arm_strb_reg() 
If DEB=3 Then Print "arm_strb_reg"
    arm_memio_strb(arm_memio_reg_op()) 
end sub

sub t16_strb_imm5() 
If DEB=3 Then Print "t16_strb_imm5"
    arm_memio_strb(t16_memio_imm5_op(_ARM_BYTE_SZ)) 
end sub

sub t16_strb_reg() 
If DEB=3 Then Print "t16_strb_reg"
    arm_memio_strb(t16_memio_reg_op()) 
end sub

'store register byte unprivileged
sub arm_strbt_imm() 
If DEB=3 Then Print "arm_strbt_imm"
    arm_memio_strbt(arm_memio_immt_op()) 
end sub

sub arm_strbt_reg() 
If DEB=3 Then Print "arm_strbt_reg"
    arm_memio_strbt(arm_memio_regt_op()) 
end sub

'store register dual
sub arm_strd_imm() 
If DEB=3 Then Print "arm_strd_imm"
    arm_memio_strd(arm_memio_immdh_op()) 
end sub

sub arm_strd_reg() 
If DEB=3 Then Print "arm_strd_reg"
    arm_memio_strd(arm_memio_regdh_op()) 
end sub

'store register halfword
sub arm_strh_imm() 
If DEB=3 Then Print "arm_strh_imm"
    arm_memio_strh(arm_memio_immdh_op()) 
end sub

sub arm_strh_reg() 
If DEB=3 Then Print "arm_strh_reg"
    arm_memio_strh(arm_memio_regdh_op()) 
end sub

sub t16_strh_imm5() 
If DEB=3 Then Print "t16_strh_imm5"
    arm_memio_strh(t16_memio_imm5_op(_ARM_HWORD_SZ)) 
end sub

sub t16_strh_reg() 
If DEB=3 Then Print "t16_strh_reg"
    arm_memio_strh(t16_memio_reg_op()) 
end sub

'subtract
sub arm_sub_imm() 
If DEB=3 Then Print "arm_sub_imm"
    arm_arith_sub(arm_data_imm_op()) 
end sub

sub arm_sub_regi() 
If DEB=3 Then Print "arm_sub_regi"
    arm_arith_sub(arm_data_regi_op()) 
end sub

sub arm_sub_regr() 
If DEB=3 Then Print "arm_sub_regr"
    arm_arith_sub(arm_data_regr_op()) 
end sub

sub t16_sub_imm3() 
If DEB=3 Then Print "t16_sub_imm3"
    arm_arith_sub(t16_data_imm3_op()) 
end sub

sub t16_sub_imm8() 
If DEB=3 Then Print "t16_sub_imm8"
    arm_arith_sub(t16_data_imm8_op()) 
end sub

sub t16_sub_reg() 
If DEB=3 Then Print "t16_sub_reg"
    arm_arith_sub(t16_data_reg_op()) 
end sub

sub t16_sub_sp7() 
If DEB=3 Then Print "t16_sub_sp7"
    arm_arith_sub(t16_data_imm7sp_op()) 
end sub

'supervisor call
sub arm_svc() 
If DEB=3 Then Print "arm_svc"
    arm_int(_ARM_VEC_SVC , _ARM_SVC ) 
end sub

sub t16_svc() 
If DEB=3 Then Print "t16_svc"
    arm_int(_ARM_VEC_SVC , _ARM_SVC ) 
end sub

'swap
sub arm_swp() 
If DEB=3 Then Print "arm_swp"
    dim as ubyte rt2 = (arm_op shr  0) and &hf 
    dim as ubyte rt  = (arm_op shr 12) and &hf 
    dim as ubyte rn  = (arm_op shr 16) and &hf 
    dim as BOOL  b   = (arm_op shr 22) and &h1 

    dim as ulong valor 

    if (b) then 
        valor = arm_readb_n(arm_r.r(rn)) 
    else
        valor = arm_read_n (arm_r.r(rn))
    endif
  
    if (b) then 
        arm_writeb_n(arm_r.r(rn), arm_r.r(rt2)) 
    else
        arm_write_n (arm_r.r(rn), arm_r.r(rt2))
    endif
  
    arm_r.r(rt) = valor 

    arm_cycles+=1  
end sub

'test equivalence
sub arm_teq_imm() 
If DEB=3 Then Print "arm_teq_imm"
    arm_logic_teq(arm_data_imm_op()) 
end sub

sub arm_teq_regi() 
If DEB=3 Then Print "arm_teq_regi"
    arm_logic_teq(arm_data_regi_op()) 
end sub

sub arm_teq_regr() 
If DEB=3 Then Print "arm_teq_regr"
    arm_logic_teq(arm_data_regr_op()) 
end sub

'test
sub arm_tst_imm() 
If DEB=3 Then Print "arm_tst_imm"
    arm_logic_tst(arm_data_imm_op()) 
end sub

sub arm_tst_regi() 
If DEB=3 Then Print "arm_tst_regi"
    arm_logic_tst(arm_data_regi_op()) 
end sub

sub arm_tst_regr() 
If DEB=3 Then Print "arm_tst_regr"
    arm_logic_tst(arm_data_regr_op()) 
end sub

sub t16_tst_rdn3() 
If DEB=3 Then Print "t16_tst_rdn3"
    arm_logic_tst(t16_data_rdn3_op()) 
end sub

'unsigned multiply accumulate long
sub arm_umlal() 
If DEB=3 Then Print "arm_umlal"
    arm_mpy_umlal(arm_mpy_op()) 
end sub

'unsigned multiply long
sub arm_umull() 
If DEB=3 Then Print "arm_umull"
    arm_mpy_umull(arm_mpy_op()) 
end sub

'undefined
sub arm_und() 
If DEB=3 Then Print "arm_und"
	 Print "Instruccion desconocida":beep
    arm_int(_ARM_VEC_UND , _ARM_UND ) 
end sub









/'
 * Decode
 '/
Dim Shared As ulongint Ptr arm_proc(1,4095)
Dim Shared As ulongint Ptr thumb_proc(2047)

Sub arm_proc_fill(arr As Any ptr , proc As any Ptr , size As Long) 
    Dim As Long i 
	 Dim As ulongint Ptr ll=arr
    for i = 0 To size-1         
        ll[i] = CULngInt(proc) 
    Next
End Sub

Sub arm_proc_set(arr As any Ptr , proc As any Ptr , op As ULong , mask As ULong , bits As Long) 
    'Bits you need on op needs to be set 1 on mask
    Dim As Long i, j 
    Dim As Long zbits = 0 
    Dim As Long zpos(bits) 
	 Dim As ulongint Ptr ll=arr
	 'Dim As ULong op2=op
    for i = 0 To bits -1        
        if ((mask And (1 Shl i)) = 0) Then zpos(zbits) = i :zbits+=1
    Next

    for i = 0 To (1 Shl zbits)-1         
        op And= mask 
        for j = 0 To zbits -1        
            op Or= ((i Shr j) And 1) Shl zpos(j) 
        Next
        'If op=865 And bits=12 Then Print 865,proc,Bin(op2,bits),Bin(mask,bits)
        'If op=801 And bits=12 Then Print 801,proc,Bin(op2,bits),Bin(mask,bits)
        ll[op] = CULngInt(proc) 
    Next

End Sub



Sub arm_proc_init() 
    'Format 27:20,7:4
    arm_proc_fill(@arm_proc(0,0), @arm_und, 4096) 
    arm_proc_fill(@arm_proc(1,0), @arm_und, 4096) 

    'Conditional
    arm_proc_set(@arm_proc(0,0), @arm_adc_imm,    &b001010100000, &b111111100000, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_adc_regi,   &b000010100000, &b111111100001, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_adc_regr,   &b000010100001, &b111111101001, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_add_imm,    &b001010000000, &b111111100000, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_add_regi,   &b000010000000, &b111111100001, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_add_regr,   &b000010000001, &b111111101001, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_and_imm,    &b001000000000, &b111111100000, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_and_regi,   &b000000000000, &b111111100001, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_and_regr,   &b000000000001, &b111111101001, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_shift_imm,  &b000110100000, &b111111100001, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_shift_reg,  &b000110100001, &b111111101001, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_b,          &b101000000000, &b111100000000, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_bic_imm,    &b001111000000, &b111111100000, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_bic_regi,   &b000111000000, &b111111100001, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_bic_regr,   &b000111000001, &b111111101001, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_bkpt,       &b000100100111, &b111111111111, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_bl,         &b101100000000, &b111100000000, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_blx_reg,    &b000100100011, &b111111111111, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_bx,         &b000100100001, &b111111111111, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_cdp,        &b111000000000, &b111100000001, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_clz,        &b000101100001, &b111111111111, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_cmn_imm,    &b001101110000, &b111111110000, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_cmn_regi,   &b000101110000, &b111111110001, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_cmn_regr,   &b000101110001, &b111111111001, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_cmp_imm,    &b001101010000, &b111111110000, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_cmp_regi,   &b000101010000, &b111111110001, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_cmp_regr,   &b000101010001, &b111111111001, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_eor_imm,    &b001000100000, &b111111100000, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_eor_regi,   &b000000100000, &b111111100001, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_eor_regr,   &b000000100001, &b111111101001, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_ldc,        &b110000010000, &b111000010000, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_ldm,        &b100000010000, &b111001010000, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_ldm_usr,    &b100001010000, &b111001010000, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_ldr_imm,    &b010000010000, &b111001010000, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_ldr_reg,    &b011000010000, &b111001010001, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_ldrb_imm,   &b010001010000, &b111001010000, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_ldrb_reg,   &b011001010000, &b111001010001, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_ldrbt_imm,  &b010001110000, &b111101110000, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_ldrbt_reg,  &b011001110000, &b111101110001, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_ldrd_imm,   &b000001001101, &b111001011111, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_ldrd_reg,   &b000000001101, &b111001011111, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_ldrh_imm,   &b000001011011, &b111001011111, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_ldrh_reg,   &b000000011011, &b111001011111, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_ldrsb_imm,  &b000001011101, &b111001011111, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_ldrsb_reg,  &b000000011101, &b111001011111, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_ldrsh_imm,  &b000001011111, &b111001011111, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_ldrsh_reg,  &b000000011111, &b111001011111, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_mcr,        &b111000000001, &b111100010001, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_mcrr,       &b110001000000, &b111111110000, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_mla,        &b000000101001, &b111111101111, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_mov_imm12,  &b001110100000, &b111111100000, 12) ' 932
    arm_proc_set(@arm_proc(0,0), @arm_mrc,        &b111000010001, &b111100010001, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_mrrc,       &b110001010000, &b111111110000, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_mrs,        &b000100000000, &b111110111111, 12) 
    
    arm_proc_set(@arm_proc(0,0), @arm_msr_imm,    &b001100100000, &b111111110000, 12) '801 
    arm_proc_set(@arm_proc(0,0), @arm_msr_imm,    &b001101100000, &b111111110000, 12) '865 este lo incluyo yo, no se si es correcto
    
    arm_proc_set(@arm_proc(0,0), @arm_msr_reg,    &b000100100000, &b111110111111, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_mul,        &b000000001001, &b111111101111, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_mvn_imm,    &b001111100000, &b111111100000, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_mvn_regi,   &b000111100000, &b111111100001, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_mvn_regr,   &b000111100001, &b111111101001, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_orr_imm,    &b001110000000, &b111111100000, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_orr_regi,   &b000110000000, &b111111100001, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_orr_regr,   &b000110000001, &b111111101001, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_qadd,       &b000100000101, &b111111111111, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_qdadd,      &b000101000101, &b111111111111, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_qdsub,      &b000101100101, &b111111111111, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_qsub,       &b000100100101, &b111111111111, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_rsb_imm,    &b001001100000, &b111111100000, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_rsb_regi,   &b000001100000, &b111111100001, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_rsb_regr,   &b000001100001, &b111111101001, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_rsc_imm,    &b001011100000, &b111111100000, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_rsc_regi,   &b000011100000, &b111111100001, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_rsc_regr,   &b000011100001, &b111111101001, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_sbc_imm,    &b001011000000, &b111111100000, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_sbc_regi,   &b000011000000, &b111111100001, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_sbc_regr,   &b000011000001, &b111111101001, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_smla__,     &b000100001000, &b111111111001, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_smlal,      &b000011101001, &b111111101111, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_smlal__,    &b000101001000, &b111111111001, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_smlaw_,     &b000100101000, &b111111111011, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_smul,       &b000101101000, &b111111111001, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_smull,      &b000011001001, &b111111101111, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_smulw_,     &b000100101010, &b111111111011, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_stc,        &b110000000000, &b111000010000, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_stm,        &b100000000000, &b111001010000, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_stm_usr,    &b100001000000, &b111001010000, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_str_imm,    &b010000000000, &b111001010000, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_str_reg,    &b011000000000, &b111001010001, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_strb_imm,   &b010001000000, &b111001010000, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_strb_reg,   &b011001000000, &b111001010001, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_strbt_imm,  &b010001100000, &b111101110000, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_strbt_reg,  &b011001100000, &b111101110001, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_strd_imm,   &b000001001111, &b111001011111, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_strd_reg,   &b000000001111, &b111001011111, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_strh_imm,   &b000001001011, &b111001011111, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_strh_reg,   &b000000001011, &b111001011111, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_sub_imm,    &b001001000000, &b111111100000, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_sub_regi,   &b000001000000, &b111111100001, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_sub_regr,   &b000001000001, &b111111101001, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_svc,        &b111100000000, &b111100000000, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_swp,        &b000100001001, &b111110111111, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_teq_imm,    &b001100110000, &b111111110000, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_teq_regi,   &b000100110000, &b111111110001, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_teq_regr,   &b000100110001, &b111111111001, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_tst_imm,    &b001100010000, &b111111110000, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_tst_regi,   &b000100010000, &b111111110001, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_tst_regr,   &b000100010001, &b111111111001, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_umlal,      &b000010101001, &b111111101111, 12) 
    arm_proc_set(@arm_proc(0,0), @arm_umull,      &b000010001001, &b111111101111, 12) 

    'Unconditional
    arm_proc_set(@arm_proc(1,0), @arm_blx_imm,    &b101000000000, &b111000000000, 12) 
    arm_proc_set(@arm_proc(1,0), @arm_cdp2,       &b111000000000, &b111100000001, 12) 
    arm_proc_set(@arm_proc(1,0), @arm_ldc2,       &b110000010000, &b111000010000, 12) 
    arm_proc_set(@arm_proc(1,0), @arm_mcr2,       &b111000000001, &b111100010001, 12) 
    arm_proc_set(@arm_proc(1,0), @arm_mcrr2,      &b110001000000, &b111111110000, 12) 
    arm_proc_set(@arm_proc(1,0), @arm_mrc2,       &b111000010001, &b111100010001, 12) 
    arm_proc_set(@arm_proc(1,0), @arm_mrrc2,      &b110001010000, &b111111110000, 12) 
    arm_proc_set(@arm_proc(1,0), @arm_pld_imm,    &b010101010000, &b111101110000, 12) 
    arm_proc_set(@arm_proc(1,0), @arm_pld_reg,    &b011101010000, &b111101110000, 12) 
    arm_proc_set(@arm_proc(1,0), @arm_stc2,       &b110000000000, &b111000010000, 12) 
End Sub

Sub thumb_proc_init() 
    'Format 15:5
    arm_proc_fill(@thumb_proc(0), @arm_und, 2048) 

    arm_proc_set(@thumb_proc(0), @t16_adc_rdn3,   &b01000001010, &b11111111110, 11) 
    arm_proc_set(@thumb_proc(0), @t16_add_imm3,   &b00011100000, &b11111110000, 11) 
    arm_proc_set(@thumb_proc(0), @t16_add_imm8,   &b00110000000, &b11111000000, 11) 
    arm_proc_set(@thumb_proc(0), @t16_add_reg,    &b00011000000, &b11111110000, 11) 
    arm_proc_set(@thumb_proc(0), @t16_add_rdn4,   &b01000100000, &b11111111000, 11) 
    arm_proc_set(@thumb_proc(0), @t16_add_sp7,    &b10110000000, &b11111000000, 11) 
    arm_proc_set(@thumb_proc(0), @t16_add_sp8,    &b10101000000, &b11111000000, 11) 
    arm_proc_set(@thumb_proc(0), @t16_adr,        &b10100000000, &b11111000000, 11) 
    arm_proc_set(@thumb_proc(0), @t16_and_rdn3,   &b01000000000, &b11111111110, 11) 
    arm_proc_set(@thumb_proc(0), @t16_asr_imm5,   &b00010000000, &b11111000000, 11) 
    arm_proc_set(@thumb_proc(0), @t16_asr_rdn3,   &b01000001000, &b11111111110, 11) 
    arm_proc_set(@thumb_proc(0), @t16_b_imm8,     &b11010000000, &b11110000000, 11) 
    arm_proc_set(@thumb_proc(0), @t16_b_imm11,    &b11100000000, &b11111000000, 11) 
    arm_proc_set(@thumb_proc(0), @t16_bic_rdn3,   &b01000011100, &b11111111110, 11) 
    arm_proc_set(@thumb_proc(0), @t16_bkpt,       &b10111110000, &b11111111000, 11) 
    arm_proc_set(@thumb_proc(0), @t16_blx,        &b01000111100, &b11111111100, 11) 
    arm_proc_set(@thumb_proc(0), @t16_blx_h1,     &b11101000000, &b11111000000, 11) 
    arm_proc_set(@thumb_proc(0), @t16_blx_h2,     &b11110000000, &b11111000000, 11) 
    arm_proc_set(@thumb_proc(0), @t16_blx_h3,     &b11111000000, &b11111000000, 11) 
    arm_proc_set(@thumb_proc(0), @t16_bx,         &b01000111000, &b11111111100, 11) 
    arm_proc_set(@thumb_proc(0), @t16_cmn_rdn3,   &b01000010110, &b11111111110, 11) 
    arm_proc_set(@thumb_proc(0), @t16_cmp_imm8,   &b00101000000, &b11111000000, 11) 
    arm_proc_set(@thumb_proc(0), @t16_cmp_rdn3,   &b01000010100, &b11111111110, 11) 
    arm_proc_set(@thumb_proc(0), @t16_cmp_rdn4,   &b01000101000, &b11111111000, 11) 
    arm_proc_set(@thumb_proc(0), @t16_eor_rdn3,   &b01000000010, &b11111111110, 11) 
    arm_proc_set(@thumb_proc(0), @t16_ldm,        &b11001000000, &b11111000000, 11) 
    
    arm_proc_set(@thumb_proc(0), @t16_ldr_imm5,   &b01101000000, &b11111000000, 11) '865
    
    arm_proc_set(@thumb_proc(0), @t16_ldr_sp8,    &b10011000000, &b11111000000, 11) 
    arm_proc_set(@thumb_proc(0), @t16_ldr_pc8,    &b01001000000, &b11111000000, 11) 
    arm_proc_set(@thumb_proc(0), @t16_ldr_reg,    &b01011000000, &b11111110000, 11) 
    arm_proc_set(@thumb_proc(0), @t16_ldrb_imm5,  &b01111000000, &b11111000000, 11) 
    arm_proc_set(@thumb_proc(0), @t16_ldrb_reg,   &b01011100000, &b11111110000, 11) 
    arm_proc_set(@thumb_proc(0), @t16_ldrh_imm5,  &b10001000000, &b11111000000, 11) 
    arm_proc_set(@thumb_proc(0), @t16_ldrh_reg,   &b01011010000, &b11111110000, 11) 
    arm_proc_set(@thumb_proc(0), @t16_ldrsb_reg,  &b01010110000, &b11111110000, 11) 
    arm_proc_set(@thumb_proc(0), @t16_ldrsh_reg,  &b01011110000, &b11111110000, 11) 
    arm_proc_set(@thumb_proc(0), @t16_lsl_imm5,   &b00000000000, &b11111000000, 11) 
    arm_proc_set(@thumb_proc(0), @t16_lsl_rdn3,   &b01000000100, &b11111111110, 11) 
    arm_proc_set(@thumb_proc(0), @t16_lsr_imm5,   &b00001000000, &b11111000000, 11) 
    arm_proc_set(@thumb_proc(0), @t16_lsr_rdn3,   &b01000000110, &b11111111110, 11) 
    arm_proc_set(@thumb_proc(0), @t16_mov_imm,    &b00100000000, &b11111000000, 11) 
    arm_proc_set(@thumb_proc(0), @t16_mov_rd4,    &b01000110000, &b11111111000, 11) 
    arm_proc_set(@thumb_proc(0), @t16_mov_rd3,    &b00000000000, &b11111111110, 11) 
    arm_proc_set(@thumb_proc(0), @t16_mul,        &b01000011010, &b11111111110, 11) 
    arm_proc_set(@thumb_proc(0), @t16_mvn_rdn3,   &b01000011110, &b11111111110, 11) 
    arm_proc_set(@thumb_proc(0), @t16_orr_rdn3,   &b01000011000, &b11111111110, 11) 
    arm_proc_set(@thumb_proc(0), @t16_pop,        &b10111100000, &b11111110000, 11) 
    arm_proc_set(@thumb_proc(0), @t16_push,       &b10110100000, &b11111110000, 11) 
    arm_proc_set(@thumb_proc(0), @t16_ror,        &b01000001110, &b11111111110, 11) 
    arm_proc_set(@thumb_proc(0), @t16_rsb_rdn3,   &b01000010010, &b11111111110, 11) 
    arm_proc_set(@thumb_proc(0), @t16_sbc_rdn3,   &b01000001100, &b11111111110, 11) 
    arm_proc_set(@thumb_proc(0), @t16_stm,        &b11000000000, &b11111000000, 11) 
    
    arm_proc_set(@thumb_proc(0), @t16_str_imm5,   &b01100000000, &b11111000000, 11) ' 801
    
    arm_proc_set(@thumb_proc(0), @t16_str_sp8,    &b10010000000, &b11111000000, 11) 
    arm_proc_set(@thumb_proc(0), @t16_str_reg,    &b01010000000, &b11111110000, 11) 
    arm_proc_set(@thumb_proc(0), @t16_strb_imm5,  &b01110000000, &b11111000000, 11) 
    arm_proc_set(@thumb_proc(0), @t16_strb_reg,   &b01010100000, &b11111110000, 11) 
    arm_proc_set(@thumb_proc(0), @t16_strh_imm5,  &b10000000000, &b11111000000, 11) 
    arm_proc_set(@thumb_proc(0), @t16_strh_reg,   &b01010010000, &b11111110000, 11) 
    arm_proc_set(@thumb_proc(0), @t16_sub_imm3,   &b00011110000, &b11111110000, 11) 
    arm_proc_set(@thumb_proc(0), @t16_sub_imm8,   &b00111000000, &b11111000000, 11) 
    arm_proc_set(@thumb_proc(0), @t16_sub_reg,    &b00011010000, &b11111110000, 11) 
    arm_proc_set(@thumb_proc(0), @t16_sub_sp7,    &b10110000100, &b11111111100, 11) 
    arm_proc_set(@thumb_proc(0), @t16_svc,        &b11011111100, &b11111111000, 11) 
    arm_proc_set(@thumb_proc(0), @t16_tst_rdn3,   &b01000010000, &b11111111110, 11) 
End Sub






Sub arm_init() 
	 ' memorias internas
    bios   = callocate(&h4000)  ' 16k
    wram   = Callocate(&h40000) ' 256k
    iwram  = Callocate(&h8000)  ' 32k
    pram   = Callocate(&h400)   ' 1k  (paleta de colores)
    vram   = Callocate(&h18000) ' 96k (video)
    oam    = Callocate(&h400)   ' 1k  (atributos de objetos)
    ' memorias externas (cartuchos)
    rom    = Callocate(&h2000000) ' max., 32mb
    eeprom = Callocate(&h2000) 
    flash  = Callocate(&h20000) 
    sram   = Callocate(&h10000) ' 32k tipico en la mayoria de cartuchos, 64k maximo

	'For f As Integer=0 To &hFFFF:sram[f]=&hFF:Next ' SRAM a 255 al inicio (pruebas)
	'For f As Integer=0 To &h1FFF:eeprom[f]=&hFF:Next ' SRAM a 255 al inicio (pruebas)
	'For f As Integer=0 To &h1FFFF:flash[f]=&hFF:Next ' SRAM a 255 al inicio (pruebas)

    arm_proc_init() 
    thumb_proc_init() 

    key_input.w = &h3ff 
    wait_cnt.w  = 0 
    arm_cycles  = 0 

    update_ws() 
End Sub

Sub arm_uninit() 
    free(bios) 
    free(wram) 
    free(iwram) 
    free(pram) 
    free(vram) 
    free(oam) 
    free(rom) 
    free(eeprom) 
    free(sram) 
    free(flash) 
End Sub







Sub t16_inc_r15() 
    if (pipe_reload) Then 
        pipe_reload = FALSE 
    Else
        arm_r.r(15) += 2
    EndIf
End Sub

Sub t16_step() 
    arm_pipe(1) = arm_fetchh(_SEQUENTIAL) 
    DEB_opcode=arm_pipe(0)
    call_thumb_proc=thumb_proc(arm_op Shr 5)
    call_thumb_proc()
    t16_inc_r15() 
End Sub

Sub arm_inc_r15() 
    if (pipe_reload) Then 
        pipe_reload = FALSE 
    Else
        arm_r.r(15) += 4
    EndIf
End Sub

Sub arm_step() 
    arm_pipe(1) = arm_fetch(_SEQUENTIAL) 

    ' depuracion
	 DEB_opcode=arm_pipe(0)
	 
    Dim As ULong proc 

    proc   = (arm_op Shr 16) And &hff0 
    proc Or= (arm_op Shr  4) And &h00f 

    Dim As Byte cond = (arm_op Shr 28) And &hFF 

    if (cond = _ARM_COND_UNCOND) Then 
    	  call_arm_proc=arm_proc(1, proc)
    	  call_arm_proc()
    ElseIf (arm_cond(cond)<>0) Then
    	  call_arm_proc=arm_proc(0, proc)
    	  call_arm_proc()
    EndIf

    arm_inc_r15() 
End Sub

Sub arm_int(ADDRESS As ULong , mode As Byte) 
    Dim As ULong cpsr = arm_r.cpsr 

    arm_mode_set(mode) 
    arm_spsr_set(cpsr) 

    'Set FIQ Disable flag based on exception
    if (ADDRESS = _ARM_VEC_FIQ) Or (ADDRESS = _ARM_VEC_RESET) Then 
        arm_flag_set(_ARM_F, TRUE)
    EndIf
  
    /'
     * Adjust PC based on exception
     *
     * Exc  ARM  Thumb
     * DA   $+8  $+8
     * FIQ  $+4  $+4
     * IRQ  $+4  $+4
     * PA   $+4  $+4
     * UND  $+4  $+2
     * SVC  $+4  $+2
     '/
    if (ADDRESS = _ARM_VEC_UND) Or (ADDRESS = _ARM_VEC_SVC) Then 
        if (arm_in_thumb()) Then 
            arm_r.r(15) -= 2 
        Else
            arm_r.r(15) -= 4
        EndIf
    EndIf

    if (ADDRESS = _ARM_VEC_FIQ) Or (ADDRESS = _ARM_VEC_IRQ) Or (ADDRESS = _ARM_VEC_PABT) Then 
        if (arm_in_thumb()) Then 
            arm_r.r(15) -= 0 ' nada?
        Else
            arm_r.r(15) -= 4
        EndIf
    EndIf

    arm_flag_set(_ARM_T, FALSE) 
    arm_flag_set(_ARM_I, TRUE) 

    arm_r.r(14) = arm_r.r(15) 
    arm_r.r(15) = ADDRESS 

    arm_load_pipe() 
End Sub

Sub arm_check_irq() 
    if (arm_flag_tst(_ARM_I)=0) And ((int_enb_m.w And 1)<>0) And ((int_enb.w And int_ack.w)<>0) Then 
        arm_int(_ARM_VEC_IRQ, _ARM_IRQ)
    EndIf
End Sub

Sub arm_reset() 
    arm_int(_ARM_VEC_RESET, _ARM_SVC) 
End Sub

Sub arm_exec(target_cycles As ULong) 
    if (int_halt) Then 
        timers_clock(target_cycles) 
        Exit Sub 
    EndIf

    while (arm_cycles < target_cycles)  
        Dim As ULong cycles = arm_cycles 
        arm_op      = arm_pipe(0) 
        arm_pipe(0) = arm_pipe(1) 

        if (arm_in_thumb()) Then 
            t16_step() 
        Else
            arm_step()
        EndIf
        
        ARM_deb()
        
        if (int_halt) Then arm_cycles = target_cycles 
        if (tmr_enb ) Then timers_clock(arm_cycles - cycles) 
    Wend

    arm_cycles -= target_cycles 
End Sub
