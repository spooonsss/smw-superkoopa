;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Super Koopa disassembly
; By nekoh
;
; If extra bit is not set this will act like the red caped swooping super koopa, if the 
; extra bit is set it will act like the rising super koopa.
;

!StarFix = 1 ; set to 1 if you want to make koopa die with star

macro localJSL(dest, rtlop, db)
	PHB			;first save our own DB
	PHK			;first form 24bit return address
	PEA.w ?return-1
	PEA.w <rtlop>-1		;second comes 16bit return address
	PEA.w <db><<8|<db>	;change db to desired value
	PLB
	PLB
	JML <dest>
?return:
	PLB			;restore our own DB
endmacro



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; sprite init JSL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                    print "INIT ",pc
                    LDA #$00 ; cannot be jumped on
                    STA !1656,X   ; / 
                    LDA !167A,X
                    ORA #$A0 ; no default player interaction, process interaction every frame
                    STA !167A,X


                    LDA !7FAB10,x
                    AND #$04
                    BEQ NotSet
                    LDY #$00                
                    LDA $D1             ; Player X position (16-bit) within the level, current frame       
                    SEC                       
                    SBC !E4,X       
                    STA $0F                   
                    LDA $D2                   
                    SBC !14E0,X     
                    BPL Return01AD41          
                    INY                            
                    TYA                       
                    STA !157C,X             
                    LDA !E4,X       ; x pos
                    AND #$10                
                    BEQ CODE_018547           


                    LDA #$80                
                    STA !1662,X   
                    LDA #$10                
                    STA !1686,X   

                    RTL                       ; Return 

CODE_018547:        INC !1534,X             
                    RTL                       ; Return 

NotSet:             LDA #$28                
                    STA !AA,X    
                    LDY #$00                
                    LDA $D1                   
                    SEC                       
                    SBC !E4,X       
                    STA $0F                   
                    LDA $D2                   
                    SBC !14E0,X     
                    BPL Return01AD41          
                    INY                            
                    TYA                       
                    STA !157C,X     
Return01AD41:       RTL                                          


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; sprite main code 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                    print "MAIN ",pc
                    PHB                       
                    PHK                       
                    PLB                             
                    JSR SuperKoopa         
                    PLB                       
                    RTL                       ; Return 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; sprite code JSL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DATA_02EB2F:        db $18,$E8

SuperKoopa:         JSR SuperKoopaGfx         
                    LDA !14C8,X             
                    CMP #$02                
                    BNE CODE_02EB49           
                    LDY #$04                
                    LDA $14     
                    AND #$04                
                    BEQ CODE_02EB44           
                    INY                       
CODE_02EB44:        TYA                       
                    STA !1602,X             
Return000:                    RTS                       ; Return 

CODE_02EB49:        LDA $9D     
                    BNE Return000          
                    JSR SubOffscreen0Bnk2   

		; "address": 98362, == '0x1803a'
		; "description": "A subroutine that makes sprites interact with each other and with the player, essentially combining $018032 and $01A7DC.",
                    ; JSL $01803A  	; interact with the player and with other sprites
                    JSL $018032|!BankB	; interact with other sprites

                    JSL $018022|!BankB   ; make the sprite move horizontally
                    LDA $1491|!Base2	; $1491 = amount to move the player along the X axis (set in the position-updating routine)
                    STA !1528,X

;CODE_01A7F7: ProcessInteract:    20 30 AD      JSR.W SubHorizPos         
;CODE_01A7FA:        A5 0F         LDA $0F                   

                    JSL $01801A ; UpdateYPosNoGrvty
                    LDA !9E,X       
                    CMP #$73                
                    ; BEQ CODE_02EB7D
                    BNE +
                    BRL CODE_02EB7D
                    +


                    LDY !157C,X     ; horizontal sprite direction table. #$00 = Right; #$01 = Left. 
                    LDA DATA_02EB2F,Y       
                    STA !B6,X    ; sprite x speed
                    JSR CODE_02EBF8         
                    LDA $13      
                    AND #$01                
                    BNE Return02EB7C          
                    LDA !AA,X    ; Sprite Y speed
                    CMP #$F0                
                    BMI Return02EB7C          
                    CLC                       
                    ADC #$FF                
                    STA !AA,X    ; Sprite Y speed



Return02EB7C:



LDA $15A0,X ; offscreen
BNE Return000

; JSL.L $01B44F      ; make Mario able to stand on top of sprite
; CODE_01B457:        20 F7 A7      JSR.W ProcessInteract      $01A7F7
; CODE_01B45A:        90 56         BCC CODE_01B4B2           
; CODE_01B45C:        B5 D8         LDA RAM_SpriteYLo,X       


%localJSL($01A7F7|!BankB, $8021, $01|!BankB)
BCC Return000 ; no contact

%SubVertPos()		; check the vertical distance between the player and the sprite

LDA $0F			; player Y position minus sprite Y position
CMP #$EC		; this value shifts the point where mario takes damage downwards. it allows the player to walk from koopa to koopa when they are x-adjacent (and spawned quickly enough)
BPL KoopaWins

%localJSL($01B45C|!BankB, $8021, $01|!BankB)
BCC Return000

; move mario down into the koopa to prevent floating in the air next frame
; 01B45C sets mario's position based on the sprite position, but it seems
; to be for platforms' clipping values of $FE vs ours of $03. so compensate here
LDA $96|!Base1 ; mario Y position
CLC
ADC #$03
STA $96|!Base1
LDA $97|!Base1 ; mario Y position high byte
ADC #$00
STA $97|!Base1

KoopaReturn:
RTS


KoopaWins:
If !StarFix == 1	; New check
LDA $1490|!Base2	; If option is set
BEQ NoStar		; Check for star
%Star()			; Kill if have
endif			;
NoStar:			;
LDA !154C,x		; if the interaction-disable timer is set...
ORA !15D0,x		; or the mole is being eaten...
ORA !1510,x		; or the extra bit is set...
BNE KoopaReturn		; return

!YoshiFix = 0
If !YoshiFix == 1	; Another new check
LDA $187A|!Base2	; If on yoshi
BEQ KoopaReturn		; It seem like it doesn't do anything, but it's not true, instead of simply hurt mario on yoshi, it makes yoshi run away.
endif

JSL $00F5B7|!BankB	; if none of the above are true, hurt the player

RTS

CODE_02EB7D:
        LDA !C2,X     
                    JSL $0086DF          ; execute_pointer

SuperKoopaPtrs:     dw CODE_02EB8D           
                    dw CODE_02EBD1           
                    dw CODE_02EBE7            ;&$FFFF           

DATA_02EB89:        db $18,$E8

DATA_02EB8B:        db $01,$FF

CODE_02EB8D:        LDA $13      
                    AND #$00                
                    STA $01                   
                    STZ $00                   
                    LDY !157C,X     
                    LDA !B6,X    
                    CMP DATA_02EB89,Y       
                    BEQ CODE_02EBAB           
                    CLC                       
                    ADC DATA_02EB8B,Y       
                    LDY $01                   
                    BNE CODE_02EBA9           
                    STA !B6,X    
CODE_02EBA9:        INC $00                   
CODE_02EBAB:        INC !151C,X             
                    LDA !151C,X             
                    CMP #$30                
                    BEQ CODE_02EBCA           
CODE_02EBB5:        LDY #$00                
                    LDA $13      
                    AND #$04                
                    BEQ CODE_02EBBE           
                    INY                       
CODE_02EBBE:        TYA                       
                    LDY $00                   
                    BNE CODE_02EBC6           
                    CLC                       
                    ADC #$06                
CODE_02EBC6:        STA !1602,X             
                    RTS                       ; Return 

CODE_02EBCA:        INC !C2,X     
                    LDA #$D0                
                    STA !AA,X    
                    RTS                       ; Return 

CODE_02EBD1:        LDA !AA,X    
                    CLC                       
                    ADC #$02                
                    STA !AA,X    
                    CMP #$14                
                    BMI CODE_02EBDE           
                    INC !C2,X     
CODE_02EBDE:        STZ $00                   
                    JSR CODE_02EBB5         
                    INC !1602,X             
                    RTS                       ; Return 

CODE_02EBE7:        LDY !157C,X     
                    LDA DATA_02EB89,Y       
                    STA !B6,X    
                    LDA !AA,X    
                    BEQ CODE_02EBF8           
                    CLC                       
                    ADC #$FF                
                    STA !AA,X    
CODE_02EBF8:        LDY #$02                
                    LDA $13      
                    AND #$04                
                    BEQ CODE_02EC01           
                    INY                       
CODE_02EC01:        TYA                       
                    STA !1602,X             
                    RTS                       ; Return 


DATA_02EC06:        db $08,$08,$10,$00,$08,$08,$10,$00
                    db $08,$10,$10,$00,$08,$10,$10,$00
                    db $09,$09,$00,$00,$09,$09,$00,$00
                    db $08,$10,$00,$00,$08,$10,$00,$00
                    db $08,$10,$00,$00,$00,$00,$F8,$00
                    db $00,$00,$F8,$00,$00,$F8,$F8,$00
                    db $00,$F8,$F8,$00,$FF,$FF,$00,$00
                    db $FF,$FF,$00,$00,$00,$F8,$00,$00
                    db $00,$F8,$00,$00,$00,$F8,$00,$00
DATA_02EC4E:        db $00,$08,$08,$00,$00,$08,$08,$00
                    db $03,$03,$08,$00,$03,$03,$08,$00
                    db $FF,$07,$00,$00,$FF,$07,$00,$00
                    db $FD,$FD,$00,$00,$FD,$FD,$00,$00
                    db $FD,$FD,$00,$00

SuperKoopaTiles:    db $C8,$D8,$D0,$E0,$C9,$D9,$C0,$E2
                    db $E4,$E5,$F2,$E0,$F4,$F5,$F2,$E0
                    db $DA,$CA,$E0,$CF,$DB,$CB,$E0,$CF
                    db $E4,$E5,$E0,$CF,$F4,$F5,$E2,$CF
                    db $E4,$E5,$E2,$CF

DATA_02EC96:        db $03,$03,$03,$00,$03,$03,$03,$00
                    db $03,$03,$01,$01,$03,$03,$01,$01
                    db $83,$83,$80,$00,$83,$83,$80,$00
                    db $03,$03,$00,$01,$03,$03,$00,$01
                    db $03,$03,$00,$01

DATA_02ECBA:        db $00,$00,$00,$02,$00,$00,$00,$02
                    db $00,$00,$00,$02,$00,$00,$00,$02
                    db $00,$00,$02,$00,$00,$00,$02,$00
                    db $00,$00,$02,$00,$00,$00,$02,$00
                    db $00,$00,$02,$00

SuperKoopaGfx:      %GetDrawInfo()
                    LDA !157C,X     
                    STA $02                   
                    LDA !15F6,X     
                    AND #$0E                
                    STA $05                   
                    LDA !1602,X             
                    ASL                       
                    ASL                       
                    STA $03                   
                    PHX                       
                    STZ $04                   
CODE_02ECF7:        LDA $03                   
                    CLC                       
                    ADC $04                   
                    TAX                       
                    LDA $01                   
                    CLC                       
                    ADC DATA_02EC4E,X       
                    STA $0301|!Base2,Y         
                    LDA SuperKoopaTiles,X   
                    STA $0302|!Base2,Y          
                    PHY                       
                    TYA                       
                    LSR                       
                    LSR                       
                    TAY                       
                    LDA DATA_02ECBA,X       
                    STA $0460|!Base2,Y      
                    PLY                       
                    LDA $02                   
                    LSR                       
                    LDA DATA_02EC96,X       
                    AND #$02                
                    BEQ CODE_02ED4D           
                    PHP                       
                    PHX                       
                    LDX $15E9|!Base2               ; X = Sprite index 
                    LDA !1534,X             
                    BEQ CODE_02ED3B           
                    LDA $13B     
                    LSR                       
                    AND #$01                
                    PHY                       
                    TAY                       
                    LDA DATA_02ED39,Y       
                    PLY                       
                    BRA CODE_02ED44           


DATA_02ED39:        db $10,$0A

CODE_02ED3B:        LDA !9E,X       
                    CMP #$72                
                    LDA #$08                
                    BCC CODE_02ED44           
                    LSR                       
CODE_02ED44:        PLX                       
                    PLP                       
                    ORA DATA_02EC96,X       
                    AND #$FD                
                    BRA CODE_02ED52           

CODE_02ED4D:        LDA DATA_02EC96,X       
                    ORA $05                   
CODE_02ED52:        ORA $64                   
                    BCS CODE_02ED5F           
                    PHA                       
                    TXA                       
                    CLC                       
                    ADC #$24                
                    TAX                       
                    PLA                       
                    ORA #$40                
CODE_02ED5F:        STA $0303|!Base2,Y          
                    LDA $00                   
                    CLC                       
                    ADC DATA_02EC06,X       
                    STA $0300|!Base2,Y         
                    INY                       
                    INY                       
                    INY                       
                    INY                       
                    INC $04                   
                    LDA $04                   
                    CMP #$04                
                    BEQ SkipJump
                    JMP CODE_02ECF7           
SkipJump:           PLX                       
                    LDY #$FF                
                    LDA #$03                
                    JSL $01B7B3    ; FinishOAMWrite
                    RTS                       ; Return 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Sprite Graphics Routine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DATA_02D003:        db $40,$B0
DATA_02D005:        db $01,$FF
DATA_02D007:        db $30,$C0,$A0,$C0,$A0,$70,$60,$B0
DATA_02D00F:        db $01,$FF,$01,$FF,$01,$FF,$01,$FF


SubOffscreen0Bnk2:  STZ $03                   ; / 
                    JSR IsSprOffScreenBnk2  ; \ if sprite is not off screen, return 
                    BEQ Return02D090          ; / 
                    LDA $5B     ; \  vertical level 
                    AND #$01                ;  | 
                    BNE VerticalLevelBnk2     ; / 
                    LDA $03                   
                    CMP #$04                
                    BEQ CODE_02D04D           
                    LDA !D8,X       ; \ 
                    CLC                       ;  | 
                    ADC #$50                ;  | if the sprite has gone off the bottom of the level... 
                    LDA !14D4,X     ;  | (if adding 0x50 to the sprite y position would make the high byte >= 2) 
                    ADC #$00                ;  | 
                    CMP #$02                ;  | 
                    BPL OffScrEraseSprBnk2    ; /    ...erase the sprite 
                    LDA !167A,X   ; \ if "process offscreen" flag is set, return 
                    AND #$04                ;  | 
                    BNE Return02D090          ; / 
CODE_02D04D:        LDA $13      
                    AND #$01                
                    ORA $03                   
                    STA $01                   
                    TAY                       
                    LDA $1A    
                    CLC                       
                    ADC DATA_02D007,Y       
                    ROL $00                   
                    CMP !E4,X       
                    PHP                       
                    LDA $1B    
                    LSR $00                   
                    ADC DATA_02D00F,Y       
                    PLP                       
                    SBC !14E0,X     
                    STA $00                   
                    LSR $01                   
                    BCC CODE_02D076           
                    EOR #$80                
                    STA $00                   
CODE_02D076:        LDA $00                   
                    BPL Return02D090          
OffScrEraseSprBnk2: LDA !14C8,X             ; \ If sprite status < 8, permanently erase sprite 
                    CMP #$08                ;  | 
                    BCC OffScrKillSprBnk2     ; / 
                    LDY !161A,X ; \ Branch if should permanently erase sprite 
                    CPY #$FF                ;  | 
                    BEQ OffScrKillSprBnk2     ; / 
                    LDA #$00                ; \ Allow sprite to be reloaded by level loading routine 
                    PHX
                    TYX
                    STA !1938,X ; / 
                    PLX
OffScrKillSprBnk2:  STZ !14C8,X             ; Erase sprite 
Return02D090:       RTS                       ; Return 

VerticalLevelBnk2:  LDA !167A,X   ; \ If "process offscreen" flag is set, return 
                    AND #$04                ;  | 
                    BNE Return02D090          ; / 
                    LDA $13      ; \ Return every other frame 
                    LSR                       ;  | 
                    BCS Return02D090          ; / 
                    AND #$01                
                    STA $01                   
                    TAY                       
                    LDA $1C    
                    CLC                       
                    ADC DATA_02D003,Y       
                    ROL $00                   
                    CMP !D8,X       
                    PHP                       
                    LDA $1D  
                    LSR $00                   
                    ADC DATA_02D005,Y       
                    PLP                       
                    SBC !14D4,X     
                    STA $00                   
                    LDY $01                   
                    BEQ CODE_02D0C3           
                    EOR #$80                
                    STA $00                   
CODE_02D0C3:        LDA $00                   
                    BPL Return02D090          
                    BMI OffScrEraseSprBnk2    
IsSprOffScreenBnk2: LDA !15A0,X 
                    ORA !186C,X 
                    RTS                       ; Return 