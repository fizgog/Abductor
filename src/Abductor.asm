; ABDUCTOR VIC20 version by Jeff Minter (Llamasoft 1982)
;
; BBC Conversion by fizgog 2024
;
; Try and make it compile very similar to the VIC20 address range
; to make it easier to disassembler
; Game is using screen address $1E00
; BBC Mode 7 uses $7C00 - $7FFF
; Mode 7 maps 1 screen byte to 1 screen byte on the VIC20
; Have replaced keyboard functions with my own and added NOPS were needed
; to pad the assembler so it still matches the VIC20
; BeebASM cannot do $00??,X as it converts to $??,X as it's more efficient
; We need to pad with NOP again, until fully dissembled.
; For the time being all VIC20 colour mapping is shunted off to
; the Beebs memory location $0900 - $09FF, eventually it will be discarded.

NATIVE_ADDR		= &101A		; address at which code will run
RELOAD_ADDR		= &1100		; address at which code will load

OFFSET			= RELOAD_ADDR - NATIVE_ADDR

INCLUDE "Constants.asm"

ORG &0
.ZP00               SKIP 1
.ZP01               SKIP 1
.spriteXPos         SKIP 1 ; ZP02
.spriteYPos         SKIP 1 ; ZP03
.currentSprite      SKIP 1 ; ZP04
.ZP05               SKIP 1
.playerInput        SKIP 1 ; ZP06
.currentPlayerXPos  SKIP 1 ; ZP07
.shipSize           SKIP 1 ; ZP08
.shipFrame          SKIP 1 ; ZP09
.ZP0A               SKIP 1
.ZP0B               SKIP 1 ; Used for ship 2 ?

.ZP0C               SKIP 4 ; DUMMY

.MissileFired       SKIP 1 ; ZP10
.Missile1           SKIP 1 ; ZP11
.Missile2           SKIP 1 ; ZP12
.MissileFrameRate   SKIP 1 ; ZP13

.ZP14               SKIP 2 ; DUMMY

.currentPlayerYPos  SKIP 1 ; ZP16
.ZP17               SKIP 1
.ZP18               SKIP 1
.ZP19               SKIP 1
.ZP1A               SKIP 1

.ZP1B               SKIP 1 ; DUMMY

.ZP1C               SKIP 1

.ZP1D               SKIP 3 ; DUMMY

.ZP20               SKIP 1 ; Pattern data storage
.ZP21               SKIP 1 ; Pattern data storage
.ZP22               SKIP 1
.ZP23               SKIP 1

.ZP24               SKIP 1 ; DUMMY

.ZP25               SKIP 1
.ZP26               SKIP 1
.ZP27               SKIP 1

.ZP28               SKIP 2 ; DUMMY

.ZP2A               SKIP 1
.ZP2B               SKIP 1
.ZP2C               SKIP 1

.ZP2D               SKIP 1 ; DUMMY

.ZP2E               SKIP 1

.ZP2F               SKIP 1 ; DUMMY

.ZP30               SKIP 1
.ZP31               SKIP 1
.ZP32               SKIP 1
.ZP33               SKIP 1
.ZP34               SKIP 1

.ZP35               SKIP 2 ; DUMMY

.ZP37               SKIP 1
.livesLeft          SKIP 1 ; ZP38

.ZP39               SKIP 3 ; DUMMY

.ZP3C               SKIP 1

.ZP3D               SKIP 3 ; DUMMY

.ZP40               SKIP 1

.ZP41               SKIP 23 ; DUMMY

.ZP58               SKIP 1

.ZP59               SKIP 23 ; DUMMY

.ZP70               SKIP 1
.ZP71               SKIP 1

.ZP72               SKIP 14 ; DUMMY

.ZP80               SKIP 1
.ZP81               SKIP 1
.missileSprite      SKIP 1 ; ZP82
.ZP83               SKIP 1
.missileXPos        SKIP 1 ; ZP84
.ZP85               SKIP 1
.missileYPos        SKIP 1 ; ZP86
.ZP87               SKIP 1 ; missile sprite ?
.ZP88               SKIP 1 ; missile sprite ?
.ZP89               SKIP 1 ; missile sprite ?
.ZPFF               SKIP 1

.endZP

; Stored as XY12C,XY12C,XY12C,XY12C ....
ORG $033C
.AlienXPosSprite
ORG $033D
.AlienYPosSprite
ORG $033E
.AlienUnknown1Sprite
ORG $033F
.AlienUnknown2Sprite
ORG $0340
.AlienCtrlSprite

; CALL SYS 4112 on the VIC20 to run
; VIC20 Version start at 101A, but jumps all over the place
ORG NATIVE_ADDR
.START

        LDA #$80                ; Not used in the Beeb
        JMP TitleScreen
        ; Return and exit game

;----------------------------------------------------------------------------
;
;----------------------------------------------------------------------------
.BeginLevel                     ; L101F  ; Get here from StartGame
        JSR ScoreHUD            ; S1055

.NextLevel                      ; L1022
        JSR ClearScreen         ; S1067
        JMP InitialiseGame      ; L1087
        ; Return

;----------------------------------------------------------------------------

        ; L1028 Title data 00000 ABDUCTOR HI:00000
.L1028
        ; EQUB $B0,$B0,$B0,$B0,$B0               ; 00000
        ; EQUB $20                               ; [SPACE]
        ; EQUB $01,$02,$03,$04,$05,$06,$07,$08   ; ABDUCTOR
        ; EQUB $20                               ; [SPACE]
        ; EQUB $3D,$3E                           ; [HIGH:]
        ; EQUB $B0,$B0,$B0,$B0,$B0               ; 00000

        EQUS "00000 ABDUCTOR H:"

.L1039  ; Highscore entry
        EQUS "00000"

.L103E  ; VIC20 Colour map for Score HUD
        ; Not required for Beeb
        EQUB $01,$01,$01,$01,$01               ; White
        EQUB $00                               ; Black
        EQUB $03,$03,$03,$03,$03,$03,$03,$03   ; Cyan
        EQUB $00                               ; Black
        EQUB $04,$04                           ; Magenta
        EQUB $07,$07,$07,$07,$07,$07           ; Yellow

;----------------------------------------------------------------------------
; ScoreHUD  ; S1055
;----------------------------------------------------------------------------
.ScoreHUD
        LDY #$16
.L1057
        LDA L1028-1,Y
        STA SCREEN_ADDRESS-1,Y      ; 7C00 Mode 7 screen address

        ; VIC 20 Colour Lookup
        ;LDA $103E-1,Y
        ;STA $95FF,Y

        NOP:NOP:NOP     ; Beeb, so not required
        NOP:NOP:NOP     ; Beeb, so not required

        DEY             ; Plotted HUD?
        BNE L1057       ; loop back
        RTS

;----------------------------------------------------------------------------
; ClearScreen  ; S1067
;----------------------------------------------------------------------------
; Clear screen below title with spaces ?
;----------------------------------------------------------------------------
.ClearScreen
        LDY #$00
.L1069
        LDA #BLANK                  ; SPACE (Why? Move outside of loop)
        STA CLEARSCREEN_ADDRESS,Y   ; STA $1E16,Y
        STA $7D00,Y                 ; STA $1F00,Y
        LDA #BLANK                  ; Why? Not required
        STA $7E00,Y                 ;
        STA $7F00,Y                 ; 
        DEY                         ;
        BNE L1069                   ; loop back

        LDY #$16
        LDA #DOTTED_LINE            ; - symbol
.L1080  ; dotted line
        STA DOTTED_LINE_ADDRESS,Y   ; STA $1FCD,Y
        DEY
        BNE L1080                   ; loop back
        RTS                         ; Return

;----------------------------------------------------------------------------
; InitialiseGame ; L1087
;----------------------------------------------------------------------------
.InitialiseGame
        ; not required for the Beeb
        LDA #$0F        ; %0000 1111
        STA $0904       ; VIC.VICCRE  ; Aux colour for multimode Lt Yellow

        ; VIC Screen is 1C00, but for the Beeb we will use Mode 7 7C00
        LDA #LO(SCREEN_ADDRESS)
        STA ZP00
        LDA #HI(SCREEN_ADDRESS)
        STA ZP01

        LDY #$17
        LDX #$00
.L1098  ; Create lookup addresses?

        LDA ZP01
        STA ZP58,X : NOP        ; Handles $00??,X

        LDA ZP00
        STA ZP40,X : NOP        ; Handles $00??,X

        CLC
        ADC #SCREEN_WIDTH       ; screen width of Mode 7 = 40
        STA ZP00                ; Low Byte $00,$28,$50,$78 etc into 0040 onwards
        LDA ZP01
        ADC #$00
        STA ZP01                ; High Byte $7C,$7D,$7E,$7F etc into 0058 onwards
        INX
        DEY
        BNE L1098       ; loop back

        LDA #$02
        STA shipSize    ; ZP08
        STA shipFrame   ; Starts with half frame ZP09

        ; Set player x and y start position to 11,20
        ; dotted line is on row 21
        ; Humanoids are on row 22
        ; VIC screen is 22x23
        LDA #PLAYER_XPOS        ; 11
        STA currentPlayerXPos   ; ZP07

        LDA #PLAYER_YPOS        ; 20
        STA currentPlayerYPos   ; ZP16

        LDA #$00
        STA MissileFired        ; ZP10 Reset missile firing
        STA Missile1            ; ZP11 Reset missile firing ship size 1 + 2  1 bullet
        STA Missile2            ; ZP12 Reset missile firing ship size 2 only 2 bullets

        JSR InitialiseHumanoids ; S144A

        LDA #$00                ; Set Humanoid Action State to 0 (SAFE)
        STA ZP71                ; Already set with InitialiseHumanoids

        LDA #$05
        STA ZP1A                ;

        LDA #$03
        STA ZP1C                ; ?

        JSR S1A80

        NOP:NOP:NOP:NOP         ; Why?

        JMP GameLoop            ; L1108
        ; Return

;----------------------------------------------------------------------------
; CalcAddress ; S10E0
;----------------------------------------------------------------------------
.CalcAddress
        LDX spriteYPos  ; ZP03
        LDY spriteXPos  ; ZP02 - not used in this function

        ;LDA $0040,X
        NOP             ; BeebEm cannot do $00??,X
        LDA ZP40,X
        STA ZP00

        ;LDA $0058,X
        NOP             ; BeebEm cannot do $00??,X
        LDA ZP58,X
        STA ZP01
        RTS

;----------------------------------------------------------------------------
; PlotSprite ; L10EF
;----------------------------------------------------------------------------
; On entry currentSprite already defined
;----------------------------------------------------------------------------
.PlotSprite
        JSR CalcAddress         ; S10E0
        LDA currentSprite       ; ZP04
        STA (ZP00),Y            ; Plot to screen address
        LDA ZP01                ;
        CLC                     ;
        ADC #$78                ; why #$78
        STA ZP01                ;
        LDA ZP05                ; 
        STA (ZP00),Y            ; Plot to screen address
        RTS                     ; Exit
;----------------------------------------------------------------------------
; GetCharacter ; S1102
;----------------------------------------------------------------------------
.GetCharacter
        JSR CalcAddress         ; S10E0
        LDA (ZP00),Y
        RTS

;----------------------------------------------------------------------------
; Game loop ; L1108
;----------------------------------------------------------------------------
.GameLoop
        JSR ProcessShip         ; S11DF
        JSR ProcessMissile      ; S126F
        JSR UpdateMissile       ; S1306
        JSR UpdateMissile2      ; S1363
        JSR ProcessSound1       ; S1421
        JSR ProcessHumanoid     ; S1489 
        JSR UpdateHumanoid      ; S159B
        JSR ProcessSound2       ; S1603
        JMP L1B00
;----------------------------------------------------------------------------
        ; This is not used
        ;EQUB $D0,$E3,$60
        BNE GameLoop ; $D0, $E3
        RTS          ; $60

;----------------------------------------------------------------------------
; ProcessKeyboard
;----------------------------------------------------------------------------
.KeyDataTable
EQUB keyUp, keyDown, keyLeft, keyRight, keyReturn
NOP

;-------------------------------------------------------------------------
; ProcessKeyboard ; S1126
;-------------------------------------------------------------------------
; playerInput contains the following
; up    = 1
; down  = 3
; left  = 4
; right = 8
; fire  = 80
;-------------------------------------------------------------------------
; On entry  :
; On exit   :
;-------------------------------------------------------------------------
.ProcessKeyboard
{
    SEI
    LDA #%01111111 : STA sysVIADataDirectionRegA

    ; Keyboard + Low
    LDA #%00000011 : STA sysVIAPortB

    LDA #KEYPRESS_NOTHING : STA playerInput

    LDX #4
.loop
    LDA KeyDataTable,X
    JSR KeyboardScanCheck
    DEX
    BPL loop

    ; Keyboard + High
    LDA #%00001011 : STA sysVIAPortB

    LDA #%11111111 : STA sysVIADataDirectionRegA
    CLI

    LDA playerInput
    RTS
}

;-------------------------------------------------------------------------
; KeyboardScanCheck
;-------------------------------------------------------------------------
; On entry  :
; On exit   :
;-------------------------------------------------------------------------
.KeyboardScanCheck
{
	STA sysVIAPortA     ; select key
	LDA sysVIAPortA	    ; read key status
	ASL A			    ; shift key status into player input bits
	ROL playerInput     ; ZP06
	RTS				    ; done.
}

;----------------------------------------------------------------------------
; UpdatePlayerShip ; S115E
;----------------------------------------------------------------------------
.UpdatePlayerShip
        JSR S11E7               ; Process Keyboard with A containing keypress
        AND #$04                ; Right pressed
        BEQ L117C               ; Yes then branch
        DEC shipFrame           ; ZP09
        BNE L1170               ; Exit
        LDA #$02                ; set single ship to 2 chars
        STA shipFrame           ; ZP09
        JMP L1171               ; Ship going left
        ; Return

.L1170
        RTS

.L1171  ; Ship going left
        DEC currentPlayerXPos   ; ZP07
        LDA currentPlayerXPos   ; ZP07
        CMP #$00                ; player min width 0
        BNE L1198               ; No
        JMP L118E               ; Use right code to move it right by 1
        ; Return

.L117C
        JMP L11F4
        ; Return

.L117F
        NOP:NOP:NOP             ; delay?

        CMP #$03
        BNE L118D               ; Exit
        LDA #$01
        STA shipFrame           ; ZP09
        JMP L118E
        ; Return

.L118D
        RTS

.L118E  ; Ship moving right
        INC currentPlayerXPos   ; ZP07
        LDA currentPlayerXPos   ; ZP07
        CMP #$14                ; 20 screen width
        BNE L1198               ; No
        DEC currentPlayerXPos   ; ZP07 set back to 19

.L1198
        JMP L11FD
        ; Return

;----------------------------------------------------------------------------
; ProcessShip1  ; L119B
;----------------------------------------------------------------------------
.ProcessShip1
        LDA shipSize            ; ZP08
        CMP #$02                ; Double Size
        BEQ DoubleShip3Chars    ; Yes then branch

.ShipSingleSize                 ; Ship Size 1
        LDA currentPlayerXPos   ; ZP07
        STA spriteXPos          ; ZP02
        LDA currentPlayerYPos   ; ZP16
        STA spriteYPos          ; ZP03

        JSR CalcAddress         ; S10E0

        LDA #BLANK              ; SPACE
        STA (ZP00),Y            ; Plot to screen address
        INY                     ; Next character space
        STA (ZP00),Y            ; Plot to screen address

        JSR UpdatePlayerShip    ; S115E

        LDA shipFrame           ; ZP09
        CMP #$02                ; Ship made of 2 halves?
        BEQ L11ED               ; Half ship, and then jumps to SingleShip2Chars ; L11CB

        ; Single ship (full)
        LDA #SINGLE_SHIP        ; + sign maybe single ship
        STA currentSprite       ; ZP04
        LDA ZP37                ;
        STA ZP05                ;
        LDA currentPlayerXPos   ; ZP07
        STA spriteXPos          ; ZP02
        JMP PlotSprite          ; L10EF
        ; Return

;----------------------------------------------------------------------------
; SingleShip2Chars ; L11CB
;----------------------------------------------------------------------------
.SingleShip2Chars 
        LDA #SINGLE_SHIPH1      ; '<' left half ship 
        STA currentSprite       ; ZP04
        LDA ZP37                ;
        STA ZP05                ;
        JSR PlotSprite          ; L10EF
        INC spriteXPos          ; ZP02
        LDA #SINGLE_SHIPH2      ; '>' right half ship 
        STA currentSprite       ; ZP04
        JMP PlotSprite          ; L10EF
        ; Return

;----------------------------------------------------------------------------
; ProcessShip ; S11DF
;----------------------------------------------------------------------------
.ProcessShip
        DEC ZP0A                ; some sort of counter
        BEQ S11E4
        RTS

.S11E4
        JMP ProcessShip1        ; L119B
        ; Return

;----------------------------------------------------------------------------
;
;----------------------------------------------------------------------------
.S11E7  ; Just call ProcessKeyboard, once dissasembled
        JSR ProcessKeyboard ; S1126
        LDA playerInput     ; Not required as ProcessKeyboard return into playerInput
        RTS
;----------------------------------------------------------------------------

.L11ED
        LDA currentPlayerXPos   ; ZP07
        STA spriteXPos          ; ZP02
        JMP SingleShip2Chars         ; L11CB return back to plotting ship
        ; Return
;----------------------------------------------------------------------------

.L11F4
        LDA playerInput
        AND #KEYPRESS_RIGHT     ; #$08
        BEQ L1198
        JMP L1202

.L11FD
        LDA currentPlayerXPos   ; ZP07
        STA spriteXPos          ; ZP02
        RTS

.L1202
        INC shipFrame           ; ZP09
        LDA shipFrame           ; ZP09
        JMP L117F
        ; Return

        RTS
        NOP                     ; not used
        NOP
        NOP

;----------------------------------------------------------------------------
; DoubleShip3Chars ; L120D
;----------------------------------------------------------------------------
.DoubleShip3Chars               ; Ship Size 2
        LDA currentPlayerYPos   ; ZP16
        STA spriteYPos          ; ZP03
        LDA currentPlayerXPos   ; ZP07
        STA spriteXPos          ; ZP02
        DEC spriteXPos          ; ZP02
        JSR CalcAddress         ; S10E0

        ; Blank 4 spaces
        LDA #BLANK              ; SPACE
        LDX #$04
.L121E
        STA (ZP00),Y            ; Plot to screen address
        INY                     ; next column
        DEX                     ;
        BNE L121E               ; loop

        ; Draw the single ship on top
        DEC currentPlayerYPos   ; ZP16
        JSR ShipSingleSize      ; S11A1

        INC currentPlayerYPos   ; ZP16
        LDA currentPlayerYPos   ; ZP16
        STA spriteYPos          ; ZP03
        LDA currentPlayerXPos   ; ZP07
        STA spriteXPos          ; ZP02
        DEC spriteXPos          ; ZP02

        LDA shipFrame           ; ZP09
        CMP #$02                ; Half a frame
        BEQ DoubleShip4Chars ; L1255               ; Yes then branch

        LDA #DOUBLE_SHIP1       ; 'A' sprite
        STA ZP0B
.L123F
        LDA ZP0B
        STA currentSprite       ; ZP04
        LDA ZP37
        STA ZP05
        JSR PlotSprite          ; L10EF
        INC ZP0B
        INC spriteXPos          ; ZP02
        LDA ZP0B
        CMP #DOUBLE_SHIP3+1     ; 'D' sprite
        BNE L123F               ; loop
        RTS

;----------------------------------------------------------------------------
; DoubleShip4Chars ; L1255
;----------------------------------------------------------------------------
.DoubleShip4Chars
        LDA #DOUBLE_SHIPH1      ; * sprite
        STA ZP0B
.L1259
        LDA ZP0B
        STA currentSprite       ; ZP04
        LDA ZP37
        STA ZP05
        JSR PlotSprite          ; L10EF
        INC ZP0B
        INC spriteXPos          ; ZP02
        LDA ZP0B
        CMP #DOUBLE_SHIPH4+1    ;
        BNE L1259               ; loop
        RTS

;----------------------------------------------------------------------------
; ProcessMissile ; S126F
;----------------------------------------------------------------------------
.ProcessMissile
        LDA ZP0A                ; counter ?
        AND #$01                ; Every other frame?
        BEQ L1276               ; Yes
        RTS                     ; Exit ProcessMissile

.L1276
        DEC MissileFrameRate    ; ZP13
        BEQ L127B               ; Fire Missile
        RTS                     ; Exit ProcessMissile

.L127B  ; Fire Missile
        LDA #$30                ; Reset Framerate
        STA MissileFrameRate    ; ZP13
        JSR ProcessKeyboard     ; S1126
        LDA playerInput         ; ZP06
        AND #KEYPRESS_FIRE      ; #$80 VIC #$10 BBC
        BNE L128D               ; Fire pressed

        LDA #$00                ;
        STA MissileFired        ; Reset missile firing
        RTS                     ; Exit ProcessMissile

.L128D  ; Fire pressed
        LDA MissileFired        ; Missile fired
        BEQ L1292               ; Yes if $00
        RTS                     ; Exit ProcessMissile

.L1292  ; Fire missile
        LDA Missile1            ; ZP11
        BEQ L12A2               ; branch if missile = 0

        LDA shipSize            ; ZP08
        CMP #$02                ; size 2?
        BEQ L129D               ; Branch if size 2
        RTS                     ; Exit ProcessMissile

.L129D
        LDA Missile2            ; ZP12
        BEQ L12D1               ; branch if missile = 0
        RTS                     ; Exit ProcessMissile

.L12A2
        LDA #$FF                ; No more firing
        STA MissileFired        ; Set missile fire to $FF
        JSR MissileSoundFX      ; S1416

        NOP:NOP

        LDA currentPlayerXPos   ; ZP07
        STA ZP80                ;
        LDA currentPlayerYPos   ; ZP16
        STA ZP81                ;
        DEC ZP81                ;
        LDA shipFrame           ; ZP09
        CMP #$02
        BEQ L12C8

        LDA #MISSILE            ; Missile sprite = "
        STA missileSprite       ; ZP82
        LDA #$36                ;
.L12C1
        STA ZP83                ;

        LDA #$01                ; Missile 1
        STA Missile1            ; Set missile fire to $01
        RTS                     ; Exit ProcessMissile

.L12C8
        LDA #MISSILE            ; Missile sprite = "
        STA missileSprite       ; ZP82
        LDA #$39                ; ?
        JMP L12C1

.L12D1
        LDA #$FF                ; Stop firing
        STA MissileFired        ; Set missile fire to $FF
        JSR MissileSoundFX      ; S1416

        NOP:NOP

        LDA currentPlayerXPos   ; ZP07
        STA missileXPos         ; ZP84
        DEC missileXPos         ; ZP84
        STA ZP85
        INC ZP85
        LDA currentPlayerYPos   ; ZP16
        STA missileYPos         ; ZP86
        DEC missileYPos         ; ZP86

        ; Frames requires for proper sprites
        LDA shipFrame           ; ZP09
        CMP #$02
        BEQ L12FD
        LDA #MISSILE            ; Missile sprite = "
        STA ZP88
        LDA #MISSILE            ; Missile sprite = "
.L12F6
        STA ZP89
        LDA #$01
        STA Missile2            ; ZP12
        RTS

.L12FD
        LDA #MISSILE            ; Missile sprite = "
        STA ZP88
        LDA #MISSILE            ; Missile sprite = "
        JMP L12F6

;----------------------------------------------------------------------------
; UpdateMissile ; S1306
;----------------------------------------------------------------------------
.UpdateMissile
        LDA MissileFrameRate    ; ZP13
        CMP #$01
        BEQ L135C
.L130C
        RTS                     ; Exit UpdateMissile

.L130D
        LDA ZP80
        STA spriteXPos          ; ZP02
        LDA ZP81
        STA spriteYPos          ; ZP03
        JSR GetCharacter        ; S1102
        CMP missileSprite       ; ZP82
        BEQ L1323
        CMP ZP83
        BEQ L132E
        JSR S17F4
.L1323
        LDA ZP83
        STA currentSprite       ; ZP04
        LDA #$07
        STA ZP05
        JMP PlotSprite          ; L10EF
        ; Return

.L132E
        LDA #BLANK              ; SPACE
        STA (ZP00),Y            ; Plot to screen address
        DEC ZP81
        LDA ZP81
        CMP #$00
        BNE L133F
        LDA #$00
        STA Missile1            ; ZP11 Reset missile fireing??
        RTS                     ; Exit UpdateMissile

.L133F
        LDA ZP81
        STA spriteYPos          ; ZP03
        JSR GetCharacter        ; S1102
        CMP #BLANK              ; SPACE
        BEQ L1351               ; Yes
        CMP #$00                ; ?
        BEQ L1351               ; Yes
        JSR S17F4
.L1351
        LDA missileSprite       ; ZP82
        STA currentSprite       ; ZP04
        LDA #$07
        STA ZP05                ; STL
        JMP PlotSprite          ; L10EF
        ; Return

.L135C
        LDA Missile1 ; ZP11
        BEQ L130C
        JMP L130D
        ; Return

;----------------------------------------------------------------------------
; UpdateMissile2 ; S1363
;----------------------------------------------------------------------------
.UpdateMissile2 
        LDA MissileFrameRate    ; ZP13
        CMP #$01                ; Odd frame rate?
        BEQ L136A               ; Yes then branch
.L1369
        RTS                     ; Exit

.L136A
        LDA Missile2            ; ZP12
        BEQ L1369               ; Exit
        LDA shipSize            ; ZP08
        CMP #$02                ; Double Ship?
        BEQ L1375               ; Yes so branch
        RTS                     ; Exit

.L1375
        LDA missileYPos         ; ZP86
        STA spriteYPos          ; ZP03
        LDA missileXPos         ; ZP84
        CMP #$9F                ; ?
        BEQ L13A1               ; Yes then branch
        STA spriteXPos          ; ZP02
        JSR GetCharacter        ; S1102
        CMP ZP87                ; Missile sprite?
        BEQ L138F
        CMP ZP88
        BEQ L139D
        JSR S17FD
.L138F
        LDA ZP88
        STA currentSprite       ; ZP04
        LDA #$07
        STA ZP05
        JSR PlotSprite          ; L10EF
        JMP L13A1

.L139D
        LDA #BLANK              ; SPACE
        STA (ZP00),Y            ; Plot to screen address
.L13A1
        LDA ZP85
        STA spriteXPos          ; ZP02
        CMP #$9F
        BEQ L13C6
        JSR GetCharacter        ; S1102
        CMP ZP87                ; Missile sprite?
        BEQ L13B7
        CMP ZP88
        BEQ L13C2
        JSR S17FD
.L13B7
        LDA ZP88
        STA currentSprite       ; ZP04
        LDA #$07
        STA ZP05
        JMP PlotSprite          ; L10EF

.L13C2
        LDA #BLANK              ; SPACE
        STA (ZP00),Y            ; Plot to screen address
.L13C6
        DEC missileYPos         ; ZP86
        LDA missileYPos         ; ZP86
        CMP #$00
        BNE L13D3
        LDA #$00
        STA Missile2            ; ZP12
        RTS                     ; Return

.L13D3
        STA spriteYPos          ; ZP03
        LDA ZP85
        CMP #$9F
        BEQ L13F4
        JSR GetCharacter        ; S1102
        CMP #BLANK              ; SPACE
        BEQ L13E9
        CMP #$00
        BEQ L13E9
        JSR S1806
.L13E9
        LDA ZP87                ; Missile sprite
        STA currentSprite       ; ZP04
        LDA #$07
        STA ZP05
        JSR PlotSprite          ; L10EF
.L13F4
        LDA missileXPos         ; ZP84
        CMP #$9F
        BNE L13FB
        RTS                     ; Return

.L13FB
        STA spriteXPos          ; ZP02
        JSR GetCharacter        ; S1102
        CMP #$00
        BEQ L140B
        CMP #BLANK              ; SPACE
        BEQ L140B
        JSR S1806
.L140B
        LDA ZP87                ; Missile sprite?
        STA currentSprite       ; ZP04
        LDA #$07
        STA ZP05
        JMP PlotSprite          ; L10EF

.MissileSoundFX                 ; S1416
        LDA #$F0                ; %1111 0000
        STA $0901               ; VIC.VICCRB  ; Sound Alto
        LDA #$F2                ; %1111 0010
        STA $0902               ; VIC.VICCRC  ; Sound Soprano
        RTS                     ; Exit

;----------------------------------------------------------------------------
; ProcessSound1 ; S1421
;----------------------------------------------------------------------------
.ProcessSound1 
        LDA MissileFrameRate    ; ZP13
        CMP #$01
        BEQ L1428
        RTS                     ; Exit

.L1428
        LDA $0902               ; VIC.VICCRC  ; Sound Soprano
        AND #$80
        BNE L1440
        RTS                     ; Exit

.L1430
        DEC $0902               ; VIC.VICCRC  ; Sound Soprano
        DEC $0901               ; VIC.VICCRB  ; Sound Alto
        RTS                     ; Exit

.L1437
        LDA #$00
        STA $0901               ; VIC.VICCRB  ; Sound Alto
        STA $0902               ; VIC.VICCRC  ; Sound Soprano
        RTS                     ; Exit

.L1440
        LDA $0902               ; VIC.VICCRC  ; Sound Soprano
        CMP #$C0
        BNE L1430
        JMP L1437
        ; Return

;----------------------------------------------------------------------------
; InitialiseHumanoids ; S144A
;----------------------------------------------------------------------------
.InitialiseHumanoids
{
        LDY #$00
.L144C
        LDA #ROW_OF_MEN         ; Row 22 position of Humanoids
        STA ZP70,Y              ; Row address
        LDA #$00
        STA ZP71,Y              ; Set Humanoid Action State to 0 (SAFE)
        INY
        INY
        CPY #$0C                ; 12
        BNE L144C               ; = 12, No then loop
        RTS                     ; Exit
}
 ;----------------------------------------------------------------------------

.L145D
        LDY #$00
.L145F
        TYA
        PHA
        JSR S146D
        PLA
        TAY
        INY
        INY
        CPY #$0C
        BNE L145F
        RTS                     ; Exit

.S146D
        LDA ZP71,Y              ; Get Humanoid Action State
        BNE L1486               ; Not 0 (SAFE) then branch
        LDA ZP70,Y              ; Row address
        STA spriteYPos          ; ZP03
        TYA
        ASL A
        STA spriteXPos          ; ZP02
        LDA #HUMANOID           ; Humanoid
        STA currentSprite       ; ZP04
        LDA #$01
        STA ZP05
        JMP PlotSprite          ; L10EF
        ; Return

.L1486
        JMP L1490
        ; Return

;----------------------------------------------------------------------------
; ProcessHumanoid ; S1489
;----------------------------------------------------------------------------
.ProcessHumanoid 
        LDA MissileFrameRate    ; ZP13
        CMP #$01
        BEQ L150B
        RTS                     ; Exit

.L1490
        STY ZP19
        LDA ZP71,Y              ; Get Humanoid Action State
        CMP #$FF                ; Humanoid Destroyed?
        BNE L149A               ; No then branch
        RTS                     ; Exit

.L149A
        CMP #$01
        BEQ L14A1
        JMP L151E
        ; Return

.L14A1
        LDA ZP70,Y              ; Row address
        CMP #ROW_OF_MEN         ; Row 22 position of Humanoids
        BNE L14BA
        STA spriteYPos          ; ZP03
        TYA
        ASL A
        STA spriteXPos          ; ZP02
        JSR CalcAddress         ; S10E0
        LDA #BLANK              ; SPACE
        JSR S17BA
        NOP
        STA ZP70,Y              ; Row address
.L14BA
        LDA ZP70,Y              ; Row address
        STA spriteYPos          ; ZP03
        TYA
        ASL A
        STA spriteXPos          ; ZP02
        JSR GetCharacter        ; S1102

        NOP:NOP:NOP             ; Delay?

        LDA #BLANK              ; SPACE
        STA (ZP00),Y            ; Plot to screen address
        DEC spriteYPos          ; ZP03
        JSR GetCharacter        ; S1102

        NOP:NOP:NOP             ; Delay?

        LDA #BLANK              ; SPACE
        STA (ZP00),Y            ; Plot to screen address

        LDA ZP19
        TAX
        TAY
        DEC ZP70,X : NOP        ; Handles $00??,X

        LDA ZP70,Y              ; Row address
        CMP #$01                ; Humanoid going up
        BNE L14ED               ; !=1 then Humanoid captured
        LDA #$02                ; Skull falling down
        STA ZP71,Y              ; Set Humanoid Action State to 2 (FALLING)
        RTS

.L14ED  ; Humanoid captured and going up
        STA spriteYPos              ; ZP03
        TYA
        ASL A
        STA spriteXPos              ; ZP02
        LDA #HUMANOID               ; Humanoid going up
        STA currentSprite           ; ZP04
        LDA #$01
        STA ZP05
        JSR PlotSprite              ; L10EF
        LDA ZP17
        STA currentSprite           ; ZP04
        LDA ZP18
        STA ZP05
        DEC spriteYPos              ; ZP03 man_y = man_y - 1
        JMP PlotSprite              ; L10EF
        ; Return

.L150B
        DEC ZP1A
        LDA ZP1A
        CMP #$00
        BEQ L1514
        RTS

.L1514
        LDA #$08
        STA ZP1A
        JMP L145D

        EQUB $B9,$71,$00
.L151E
        CMP #$03
        BEQ L1525
        JMP L155F

.L1525
        LDA ZP70,Y              ; Row address
        STA spriteYPos          ; ZP03
        TYA
        ASL A
        STA spriteXPos          ; ZP02
        JSR GetCharacter        ; S1102
        CMP #BLANK              ; SPACE
        BNE L1536
        RTS

.L1536
        CMP #$78        ; ASCII 'x' #$21
        BNE L1545
        LDA #$79        ; ASCII 'y' #$22
.L153C
        STA currentSprite ; ZP04
        LDA #$01
        STA ZP05
        JMP PlotSprite ; L10EF

.L1545
        CMP #$79        ; ASCII 'y' #$22
        BNE L154E
        LDA #$7A        ; ASCII 'z' #$23
        JMP L153C

.L154E
        CMP #$7A                ; ASCII 'z' #$23
        BEQ L1553
        RTS

.L1553
        LDA #$FF                ; Humanoid captured value
        LDY ZP19
        STA ZP71,Y              ; Set Humanoid Action State to FF (CAPTURED)
        LDA #BLANK              ; SPACE
        JMP L153C

.L155F
        LDA ZP71,Y              ; Get Humanoid Action State
        CMP #$04
        BEQ L1567
        RTS

.L1567
        LDA ZP70,Y              ; Row address
        STA spriteYPos          ; ZP03
        TYA
        ASL A
        STA spriteXPos          ; ZP02
        LDA #BLANK              ; SPACE
        STA currentSprite       ; ZP04
        JSR PlotSprite          ; L10EF
        LDA ZP19
        TAX
        TAY
        INC ZP70,X : NOP        ; Handles $00??,X
        LDA ZP70,Y              ; Row address
        CMP #$15
        BEQ L1590
        LDA #HUMANOID           ; #$1A; Humanoid
        STA currentSprite       ; ZP04
        LDA #$01
        STA ZP05
        JMP L18CA
        ; Return

.L1590
        LDA #ROW_OF_MEN         ; Row 22 position of Humanoids
        STA ZP70,Y              ; Row address
        LDA #$00
        JMP SaveHumanoid             ; L1A71

        RTS

;----------------------------------------------------------------------------
; UpdateHumanoid ; S159B
;----------------------------------------------------------------------------
.UpdateHumanoid 
        LDA MissileFrameRate    ; ZP13
        CMP #$02
        BEQ L15F7
        RTS                     ; Exit

.L15A2
        LDY #$00
.L15A4
        LDA ZP71,Y              ; Get Humanoid Action State
        CMP #$02
        BNE L15AE
        JSR S15B5
.L15AE
        INY
        INY
        CPY #$0C
        BNE L15A4
        RTS                     ; Exit

.S15B5
        LDA ZP70,Y              ; Row address
        CMP #$01
        BNE L15C1
        LDA #$C0
        STA $0903               ; VIC.VICCRD  ; Sound Noise
.L15C1
        STY ZP19
        LDA ZP70,Y              ; Row address
        STA spriteYPos          ; ZP03
        TYA
        ASL A
        STA spriteXPos          ; ZP02
        LDA #BLANK              ; SPACE
        STA currentSprite       ; ZP04
        JSR CheckAlienHitShip   ; L19F4
        LDA ZP19
        TAY
        TAX
        INC ZP70,X : NOP        ; Handles $00??,X

        LDA ZP70,Y              ; Row address
        CMP #$15                ; row 21
        BEQ L15F1               ; Yes
        STA spriteYPos          ; ZP03

        LDA #SKULL              ; Skull coming down
        STA currentSprite       ; ZP04

        LDA #$01
        STA ZP05

        JSR CheckAlienHitShip   ; L19F4
        LDY ZP19
        RTS                     ; Exit

.L15F1
        LDA #$FF
        STA ZP71,Y              ; Set Humanoid Action State to FF (CAPTURED)
        RTS                     ; Exit

.L15F7
        DEC ZP1C
        BEQ L15FC
        RTS                     ; Exit

.L15FC
        LDA #$03
        STA ZP1C
        JMP L15A2
        ; Return

;----------------------------------------------------------------------------
; ProcessSound2 ; S1603
;----------------------------------------------------------------------------
.ProcessSound2
        LDA MissileFrameRate    ; ZP13
        CMP #$02
        BEQ L160A

.L1609
        RTS                     ; Exit

.L160A
        LDA $0903               ; VIC.VICCRD  ; Sound Noise
        AND #$80
        BEQ L1609               ; Exit
        LDA $0903               ; VIC.VICCRD  ; Sound Noise
        SBC #$04
        STA $0903               ; VIC.VICCRD  ; Sound Noise
        RTS                     ; Exit
;----------------------------------------------------------------------------
;
;----------------------------------------------------------------------------
.S161A
        LDA ZP0A
        AND #$01
        BEQ L1621
        RTS                     ; Exit

.L1621
        DEC ZP25
        BEQ L1626
        RTS                     ; Exit

.L1626
        JSR S1705
        NOP
        INC ZP26
        LDY ZP26
        CPY #$05
        BNE L1636
        LDY #$01
        STY ZP26
.L1636
        LDA ($20),Y
        STA ZP17
        LDA ZP18
        STA ZP18                ; Why?
        JMP L19E2
        ; Return

.L1641
        TYA
        PHA
        ASL A
        ASL A
        JSR S173E
        JSR S1651
        PLA
        TAY
        DEY
        BNE L1641
        RTS                     ; Exit

.S1651
        STX ZP27
        LDA AlienUnknown1Sprite,X
        BNE L1659
        RTS                     ; Exit

.L1659
        CMP #$80
        BNE L1660
        JMP L17C1
        ; Return

.L1660
        LDA AlienXPosSprite,X
        STA spriteXPos              ; ZP02
        LDA AlienYPosSprite,X
        STA spriteYPos              ; ZP03

        LDA #BLANK                  ; SPACE
        STA currentSprite           ; ZP04
        JSR CheckAlienHitShip       ; L19F4

        ; Alien FrameCount 1,2,3 ?
        LDX ZP27
        INC AlienCtrlSprite,X
        LDA AlienCtrlSprite,X
        CMP #$03
        BNE L1685
        LDA #$01
        STA AlienCtrlSprite,X
        JMP L1691

.L1685
        LDY AlienUnknown2Sprite,X
        LDA ($20),Y
        ROR A
        ROR A
        ROR A
        ROR A
        JMP L16B7
        ; Return

.L1691
        INC AlienUnknown2Sprite,X
        LDY AlienUnknown2Sprite,X
        LDA ($20),Y
        BNE L16B7
        LDA #$80
        STA AlienUnknown1Sprite,X
        LDA ZP22
        JSR S1975

        INC ZP22
        LDA ZP22
        CMP #$06
        BNE L16B2
.L16AD
        LDA #$00
        STA ZP22
        RTS                     ; Exit

.L16B2
        CMP #$00
        BEQ L16AD
        RTS                     ; Exit

.L16B7
        STA ZP00
        AND #$01
        BEQ L16C0
        DEC AlienXPosSprite,X
.L16C0
        LDA ZP00
        AND #$02
        BEQ L16C9
        INC AlienXPosSprite,X
.L16C9
        LDA ZP00
        AND #$04
        BEQ L16D2
        DEC AlienYPosSprite,X
.L16D2
        LDA ZP00
        AND #$08
        BEQ S16DB
        INC AlienYPosSprite,X
.S16DB
        LDA AlienYPosSprite,X
        CMP #$15
        BNE L16E7
        LDA #$01
        STA AlienYPosSprite,X
.L16E7
        CMP #$00
        BNE L16F0

        LDA #$14
        STA AlienYPosSprite,X
.L16F0
        LDA AlienXPosSprite,X
        STA spriteXPos          ; ZP02
        LDA AlienYPosSprite,X
        STA spriteYPos          ; ZP03

        LDA ZP17                ; 
        STA currentSprite       ; ZP04
        LDA ZP18
        STA ZP05
        JMP CheckAlienHitShip   ; L19F4

.S1705
        DEC ZP2C
        BNE L1710
        LDA #$02
        STA ZP2C
        JMP L1739
        ; Return

.L1710
        LDA ZP2A
        BEQ L1739
        DEC ZP2A
        INC ZP23
        LDA ZP23
        CLC
        ASL A
        ASL A
        ADC ZP23
        TAY
        LDA #$00
        STA AlienXPosSprite,Y
        LDA #$02
        STA AlienYPosSprite,Y

        LDA #$FF
        STA AlienUnknown1Sprite,Y
        LDA #$05
        STA AlienUnknown2Sprite,Y

        LDA #$01
        STA AlienCtrlSprite,Y
.L1739
        LDA ZP34
        STA ZP25
        RTS                     ; Exit

.S173E
        STY ZP00
        ADC ZP00
        TAX
        RTS                     ; Exit

.L1744
        LDA AlienXPosSprite,X
        STA spriteXPos          ; ZP02
        LDA AlienYPosSprite,X
        STA spriteYPos          ; ZP03

        LDA #BLANK              ; SPACE
        STA currentSprite       ; ZP04
        JSR PlotSprite          ; L10EF

        LDX ZP27
        LDA AlienUnknown2Sprite,X
        JMP L1765
        ; Return

;----------------------------------------------------------------------------
; GameOver ; L175D
;----------------------------------------------------------------------------
.GameOver
        LDA #$01                ; Set lives to 1
        STA livesLeft           ; ZP38
        JMP LoseLife            ; L1A04
        ; Return

;----------------------------------------------------------------------------
        NOP                     ; Not used
;----------------------------------------------------------------------------
;
;----------------------------------------------------------------------------
.L1765
        CLC
        ASL A
        ASL A

        NOP:NOP:NOP:NOP:NOP     ; Delay?

        STA ZP2E
        LDA AlienXPosSprite,X
        CMP ZP2E
        BEQ L1781
        BMI L177E
        DEC AlienXPosSprite,X
        DEC AlienXPosSprite,X
.L177E
        INC AlienXPosSprite,X
.L1781
        INC AlienYPosSprite,X
        JSR S17B4
        LDA AlienXPosSprite,X
        CMP ZP2E
        BEQ L178F
        RTS                     ; Exit

.L178F
        LDA AlienYPosSprite,X
        CMP #$14
        BEQ L1797
        RTS                     ; Exit

.L1797
        LDA AlienUnknown2Sprite,X
        CMP #$FF
        BNE L179F
        RTS                     ; Exit

.L179F
        LDA #BLANK              ; SPACE
        STA ZPFF                ; Does nothing AFAIK?

        LDA #$00
        STA AlienUnknown1Sprite,X
        LDA AlienUnknown2Sprite,X
        CLC
        ASL A
        TAX
        LDA #$01
        STA ZP71,X              ; Set Humanoid Action State to 1 (TAKEN)
        NOP                     ; Handles $00??,X
        RTS                     ; Exit

.S17B4
        JSR S16DB
        LDX ZP27
        RTS                     ; Exit

.S17BA
        STA (ZP00),Y            ; Plot to screen address
        LDY ZP19
        LDA #$14
        RTS                     ; Exit

.L17C1
        LDA AlienUnknown2Sprite,X
        CMP #$22
        BEQ L17CB
        JMP L1744
        ; Return

.L17CB
        LDA AlienCtrlSprite,X
        STA currentSprite       ; ZP04
        LDA #$01
        STA ZP05

        LDA AlienXPosSprite,X
        STA spriteXPos          ; ZP02
        LDA AlienYPosSprite,X
        STA spriteYPos          ; ZP03

        JSR PlotSprite          ; L10EF
        LDX ZP27
        DEC AlienCtrlSprite,X
        LDA AlienCtrlSprite,X
        CMP #$1F
        BEQ HitAlien            ; L17EE
        RTS                     ; Exit

.HitAlien                       ; L17EE
        LDA #$00
        JMP DestroyAlien        ; L1A65
        ; Return

        RTS                     ; Exit but not used
.S17F4
        STA ZP30
        LDA #$01
        STA ZP31
        JMP L180C
        ; Return

.S17FD
        STA ZP30
        LDA #$02
        STA ZP31
        JMP L180C
        ; Return

.S1806
        STA ZP30
        LDA #$03
        STA ZP31
.L180C
        LDA ZP30
        CMP ZP17
        BEQ L1815
        JMP L1892
        ; Return

.L1815
        LDY ZP23
.L1817
        TYA
        STY ZP32
        ASL A
        ASL A
        CLC
        ADC ZP32
        TAX
        LDA AlienXPosSprite,X
        CMP spriteXPos          ; ZP02
        BEQ L182D
.L1827
        NOP
        NOP
        DEY
        BNE L1817
        RTS                     ; Exit

.L182D
        LDA AlienYPosSprite,X
        CMP spriteYPos          ; ZP03
        BNE L1827
        LDA #$80
        STA AlienUnknown1Sprite,X

        LDA #$22
        STA AlienUnknown2Sprite,X
        LDA #$23
        STA AlienCtrlSprite,X
        LDA #$F8
        STA $0903               ; VIC.VICCRD      ; Sound Noise

        NOP:NOP:NOP             ; Delay?
.L184B
        LDA ZP31
        CMP #$01
        BNE L1876
        LDA #$00
        STA Missile1            ; ZP11        ; Reset missile fireing??
        PLA                     ; Why?
        PLA                     ; WHY?
        RTS                     ; Exit

        ; Data?
        ; 1858
        EQUB $C9,$02,$D0,$07,$A9,$FF,$85,$84
        ; 1860
        EQUB $4C,$67,$18,$A9,$FF,$85,$85,$A5,$84,$C9,$FF,$F0,$03,$68,$68,$60
        ; 1870
        EQUB $A5,$85,$C9,$FF,$D0,$F7
.L1876
        LDA #$00
        STA Missile2            ; ZP12

        PLA                     ; Why?
        PLA                     ; Why?

        LDA missileXPos         ; ZP84
        STA spriteXPos          ; ZP02
        LDA missileYPos         ; ZP86
        STA spriteYPos          ; ZP03
        LDA #BLANK              ; SPACE
        STA currentSprite       ; ZP04
        JSR PlotSprite          ; L10EF
        LDA ZP85
        STA spriteXPos          ; ZP02
        JMP PlotSprite          ; L10EF
        ; Return
.L1892
        CMP #HUMANOID           ; #$1A; Humanoid
        BEQ L1897               ; = Humanoid then branch
        RTS                     ; Exit

.L1897
        LDY #$00
.L1899
        LDA ZP70,Y              ; Row address
        CMP spriteYPos          ; ZP03
        BEQ L18A6
.L18A0
        INY
        INY
        CPY #$0C
        BNE L1899
.L18A6
        TYA
        CLC
        ASL A
        CMP spriteXPos          ; ZP02
        BNE L18A0
        LDA #$04
        STA ZP71,Y              ; Set Humanoid Action State to 4 (UNKNOWN)
        JSR S18D2
        LDA #$F8
        STA $0903               ; VIC.VICCRD      ; Sound Noise
        JMP L184B
        ; Return

        ; 18BD
        EQUB $01,$D0,$05,$A9,$00,$85,$11,$60,$A9,$00,$85,$12,$60

.L18CA
        LDA ZP70,Y              ; Row address
        STA spriteYPos          ; ZP03
        JMP PlotSprite          ; L10EF
        ; Return
.S18D2
        LDA ZP70,Y              ; Row address
        STA spriteYPos          ; ZP03

        TYA
        ASL A
        STA spriteXPos          ; ZP02

        LDA #BLANK              ; SPACE
        STA currentSprite       ; ZP04
        DEC spriteYPos          ; ZP03
        JMP PlotSprite          ; L10EF
        ; Return

;----------------------------------------------------------------------------
;
;----------------------------------------------------------------------------
.L18E4
        JSR S19CF               ; returns with A = #$E8
        NOP                     ; Delay?
.L18E8
        CLC
        ADC #$28
        CMP #$00
        BEQ L192A

        NOP:NOP:NOP:NOP:NOP     ; Delay?

        DEY
        BNE L18E8
        
        STA ZP20                ; Store $00
        DEC ZP20                ; ZP20 = $FF

        LDA #$00
        STA ZP23                ; Store $00 in ZP23

        LDA ZP33                ; 
        AND #$07                ; 0 - 7 
        BNE L1907               ; not 0 then branch
        LDA #$01                ; set to 1
.L1907
        STA ZP18                ; must be a value between 1 and 7

        LDA #$80
        SBC ZP33
        SBC ZP33

        NOP:NOP:NOP:NOP         ; Delay?

        STA ZP34
        LDA ZP33
        ASL A
        BNE L192F
.L191A
        LDA #$01
        STA shipSize            ; ZP08

        LDA ZP33
        CMP #$05
        BPL L1925
        RTS                     ; Exit

.L1925
        LDA #$02
        STA shipSize            ; ZP08
        RTS                     ; Exit

.L192A
        LDA #$10
        JMP L18E8
        ; Return

.L192F
        STA ZP2A
        STA ZP2B
        JMP L191A
        ; Return

;----------------------------------------------------------------------------
;
;----------------------------------------------------------------------------
.S1936
        LDA ZP25
        CMP #$01
        BEQ L193D
        RTS                     ; Exit

.L193D
        LDY ZP23
.L193F
        TYA
        STA ZP00
        CLC
        ASL A
        ASL A
        ADC ZP00
        TAX
        LDA AlienUnknown1Sprite,X
        BNE L1953
        DEY
        BNE L193F
        JMP L1954
        ; Return

.L1953
        RTS                     ; Exit

.L1954
        LDA ZP2A
        BNE L1953
        LDY #$00
.L195A
        LDA ZP71,Y              ; Get Humanoid Action State
        BEQ L1964
        CMP #$FF
        BEQ L1964
        RTS                     ; Exit

.L1964
        INY
        INY
        CPY #$0C
        BNE L195A
        INC ZP33
        LDA ZP33
        JSR S1B13
        NOP                     ; Delay?
        JMP L18E4
        ; Return

.S1975
        LDY #$00
.L1977
        LDA ZP71,Y              ; Get Humanoid Action State
        BEQ L1997
.L197C
        INY
        INY
        CPY #$0C
        BNE L1977
        LDA #$00
        STA AlienXPosSprite,X
        LDA #$02
        STA AlienYPosSprite,X
        LDA #$FF
        STA AlienUnknown1Sprite,X
        LDA #$05
        STA AlienUnknown2Sprite,X
        RTS                     ; Exit

.L1997
        TYA
        ROR A
        STA spriteXPos          ; ZP02
        STX currentSprite       ; ZP04
        LDY ZP23
.L199F
        TYA
        CLC
        STA spriteYPos          ; ZP03
        ASL A
        ASL A
        ADC spriteYPos          ; ZP03
        TAX
        CPX currentSprite       ; ZP04
        BEQ L19B3
        LDA AlienUnknown2Sprite,X
        CMP spriteXPos          ; ZP02
        BEQ L19BE
.L19B3
        DEY
        BNE L199F
        LDA spriteXPos          ; ZP02
        LDX currentSprite       ; ZP04
        STA AlienUnknown2Sprite,X
        RTS                     ; Exit

.L19BE
        LDA AlienUnknown1Sprite,X
        CMP #$00
        BEQ L19B3
        LDX currentSprite       ; ZP04
        LDA spriteXPos          ; ZP02
        CLC
        ASL A
        TAY
        JMP L197C
        ; Return

;----------------------------------------------------------------------------
;
;----------------------------------------------------------------------------
.S19CF
        LDY #$C0
        LDA #$00
.L19D3
        STA $033B,Y             ; Clear to 0 between AlienXPosSprite - $03FB
        DEY
        BNE L19D3
        LDY ZP33
        LDA #$00
        STA ZP23
        LDA #$E8
.L19E1
        RTS                     ; Exit

;----------------------------------------------------------------------------

.L19E2
        LDY ZP23
        TYA
        BEQ L19E1
        JMP L1641
        ; Return

.L19EA  ; Ship characters
        ;EQUB $24,$25,$26,$27,$28,$29,$2A,$2B,$2C,$2D
        ; NB Beeb Sprites won't need to handle halves
        EQUB SINGLE_SHIP,   SINGLE_SHIPH1, SINGLE_SHIPH2  
        EQUB DOUBLE_SHIP1,  DOUBLE_SHIP2,  DOUBLE_SHIP3
        EQUB DOUBLE_SHIPH1, DOUBLE_SHIPH2, DOUBLE_SHIPH3, DOUBLE_SHIPH4


;----------------------------------------------------------------------------
; CheckAlienHitShip ; L19F4
;----------------------------------------------------------------------------
.CheckAlienHitShip
        JSR GetCharacter        ; returns with characrer in A ;S1102

        LDY #$09                ; 9 parts possible parts to ship
.L19F9
        CMP L19EA-1,Y
        BEQ LoseLife            ; L1A04
        DEY
        BNE L19F9

        JMP PlotSprite          ; L10EF
        ; Return

;----------------------------------------------------------------------------
; LoseLife ; L1A04
;----------------------------------------------------------------------------
.LoseLife 
        LDX #$F6
        TXS
        LDA #$00
        STA $0900               ; VIC.VICCRA  ; Sound Bass
        STA $0901               ; VIC.VICCRB  ; Sound Alto
        STA $0902               ; VIC.VICCRC  ; Sound Soprano
        STA playerInput         ; ZP06
        LDA #$90
        STA $0903               ; VIC.VICCRD  ; Sound Noise

        ; Stop keyboard processing
        ;LDA #$60               ; RTS
        ;STA ProcessKeyboard    ; S1126 ; Turn off keyboard

        ; Replaced above with NOP's as Beeb using it's own keyboard code
        NOP:NOP                 
        NOP:NOP:NOP     
.L1A1E
        LDA #$00
        STA ZP37
.L1A22
        JSR S11E4
        DEC ZP37
        BNE L1A22
        DEC $0904               ; VIC.VICCRE  ; Sound Volume
        BNE L1A1E

        NOP:NOP                 ; Delay?

        JSR S11E4

        ;LDA #$78               ; SEI
        ;STA ProcessKeyboard    ; S1126 ; Turn on keyboard

        ; Replaced above with NOP's as Beeb using it's own keyboard code
        NOP:NOP
        NOP:NOP:NOP

        DEC livesLeft           ; ZP38
        BEQ L1A43

        LDA #$03
        STA ZP37
        JMP NextLevel           ; L1022

.L1A43  ; Game over
        JMP ResetGame           ; L1AE3
        ; Return

;----------------------------------------------------------------------------
; AddScore ; L1A46
;----------------------------------------------------------------------------
.AddScore
        TXA
        PHA
        LDX #$04
.L1A4A
        INC SCREEN_ADDRESS-1,X  ; increase score by 1
        LDA SCREEN_ADDRESS-1,X
        CMP #$3A                ; #$BA                ; A (10) character
        BNE L1A5C               ; not reached 9
        LDA #$30                ; #$B0                ; 0 character
        STA SCREEN_ADDRESS-1,X
        DEX                     ; move to next score position
        BNE L1A4A
.L1A5C
        LDX #$04
        DEC ZP3C
        BNE L1A4A
        JMP CheckHighscore      ; L1B1F
        ; Return

;----------------------------------------------------------------------------
; DestroyAlien ; L1A65
;----------------------------------------------------------------------------
.DestroyAlien
        STA AlienUnknown1Sprite,X
        LDA ZP18
        AND #$07
        STA ZP3C                ; 0 - 3
        JMP AddScore            ; L1A46
        ; Return

;----------------------------------------------------------------------------
; SaveHumanoid ; L1A71
;----------------------------------------------------------------------------
.SaveHumanoid
        STA ZP71,Y              ; Set Humanoid Action State to 0 (SAFE)
        LDA #$14
        STA ZP3C                ; 20
        JMP AddScore            ; L1A46
        ; Return

 ;----------------------------------------------------------------------------

        NOP:NOP:NOP:NOP:NOP     ; Why? some sort of filler?

;----------------------------------------------------------------------------
;
;----------------------------------------------------------------------------
.S1A80  ; $1B37 ? Pattern Data = $1B38
        LDA #LO(PatternData-1)
        STA ZP20
        LDA #HI(PatternData-1)
        STA ZP21         

        LDA #$00
        STA ZP22
        STA ZP22                ; Duplicate ???

        LDA #$A0
        STA ZP25

        LDA #$01
        STA ZP26

        LDA #$02
        STA ZP2C

        LDA #$02                ; Not required as A is already #$02
        STA ZP2A
        STA ZP2B

        LDA #$01
        STA ZP26

        LDA #$00
        STA ZP23
        LDA ZP33                ; Why?
        STA ZP33                ; Why?

        JSR L18E4               ; 
        RTS                     ; Exit, coukd have used a JMP instead

;----------------------------------------------------------------------------
;
;----------------------------------------------------------------------------
.L1AB0
        LDY #$00
.L1AB2
        LDA ZP71,Y              ; Get Humanoid Action State
        CMP #$FF                ; Humanoid Destroyed?
        BNE L1AC2               ; No so branch
        INY                     ;
        INY                     ; 
        CPY #$0C                ; Check all humanoids
        BNE L1AB2               ; loop back

        JMP GameOver            ; All Humanoids destroyed
        ; Return

.L1AC2  ; Continue game
        JMP GameLoop            ; L1108
        ; Return

;----------------------------------------------------------------------------
; 
;----------------------------------------------------------------------------
.L1AC5
        LDA MissileFrameRate    ; ZP13
        CMP #$01                ; Every odd frame
        BEQ L1AB0               ; Yes so branch
        JMP GameLoop            ; L1108
        ; Return

;----------------------------------------------------------------------------
        NOP                     ; not used
        RTS                     ; not used
;----------------------------------------------------------------------------
; Start Game ; L1AD0
;----------------------------------------------------------------------------
.StartGame
        LDA #$02
        STA ZP33                ; ?

        LDA #$03
        STA ZP37                ; ?
        STA livesLeft           ; Number of lives? ; ZP38
        JMP BeginLevel          ; L101F ; jump back to begining of code
        ; Return

;----------------------------------------------------------------------------
; TitleScreen ; L1ADD
;----------------------------------------------------------------------------
; Simple title screen showing hud and copyright and bottom row of dashes
;----------------------------------------------------------------------------
.TitleScreen
        ;STA $0291              ; Store #$80 into Extended zero page $0291
        NOP:NOP:NOP             ; not needed for the Beeb
        JSR ScoreHUD            ; S1055 Display title

.ResetGame                      ; L1AE3
        JSR ClearScreen         ; S1067 Clear screen below title

        ; Draw Copyright
        LDY #$0A
.L1AE8
        LDA L1B09-1,Y           ; (C)JCM1982
        STA COPYRIGHT_ADDRESS,Y ; STA $1E47,Y

        ;LDA #$01               ; White font
        ;STA $9647,Y            ; Generate 10 $01 at $9648 - $9651

        NOP:NOP
        NOP:NOP:NOP             ; Above not required for Beeb

        DEY
        BNE L1AE8               ; loop

.L1AF6  ; loop until fire pressed
        JSR ProcessKeyboard     ; S1126
        AND #KEYPRESS_FIRE      ; Test for the Beeb's Return Key
        BEQ L1AF6               ; fire not pressed so loop
        JMP StartGame           ; L1AD0 ; fire pressed
        ; Return

;----------------------------------------------------------------------------
;
;----------------------------------------------------------------------------
.L1B00
        JSR S161A
        JSR S1936
        JMP L1AC5
        ; Return

.L1B09  ;
        ;EQUB $A8,$83,$A9,$B1,$B9,$B8,$B2,$8A,$83,$8D
        EQUS "(C)1982JCM"       ; Use Beebs Character Set

.S1B13
        CMP #$11                ;
        BEQ L1B1A               ;
.L1B17
        STA ZP33                ;
        RTS                     ; Exit

.L1B1A
        LDA #$0E
        JMP L1B17
        ; Return

;----------------------------------------------------------------------------
; CheckHighscore ; L1B1F
;----------------------------------------------------------------------------
.CheckHighscore
        PLA                         ; Why? Not used
        TAX                         ; Why? Not used
        LDY #$00
.L1B23
        LDA HIGHSCORE_ADDRESS,Y     ;
        CMP SCREEN_ADDRESS,Y        ;
        BMI L1B33                   ; New highscore
        BNE L1B32                   ; Exit
        INY                         ; Next character
        CPY #$05                    ; last character compared?
        BNE L1B23                   ; No then loop back
.L1B32
        RTS                         ; Exit

.L1B33  ; Call UpdateHighscore
        LDY #$05
        JMP UpdateHighscore         ; L1CE0 (Inside Fonts.asm)
        ; Return

;----------------------------------------------------------------------------
; Pattern Wave Data? ; L1B38
;----------------------------------------------------------------------------
; 5 waves of data
; Each wave is terminated by $00
; first 4 bytes are alien character frames (max 4 frames)
;----------------------------------------------------------------------------
.PatternData
        ;EQUB $09,$0A,$0B,$0C ; alien sprite1 frame 1-4
        EQUS "1111"             ; Use Beebs character set
        EQUB $33,$62,$22,$22
        EQUB $22,$22,$22,$22,$22,$2A,$AA,$89
        EQUB $91,$15,$6A,$A8,$88,$88,$99,$91
        EQUB $55,$51,$98,$88,$88,$A2,$26,$44
        EQUB $45,$55,$55,$54,$46,$66,$62,$00

        ;EQUB $0D,$0E,$0F,$0E
        EQUS "2222"             ; Use Beebs character set
        EQUB $33,$88,$88,$22
        EQUB $22,$88,$88,$22,$22,$AA,$66,$AA
        EQUB $66,$66,$66,$64,$45,$11,$91,$91
        EQUB $91,$91,$91,$91,$91,$11,$54,$46
        EQUB $2A,$88,$88,$88,$88,$88,$A2,$00

        ;EQUB $10,$11,$12,$13
        EQUS "3333"             ; Use Beebs character set
        EQUB $33,$22,$26,$44
        EQUB $44,$44,$62,$22,$2A,$AA,$AA,$AA
        EQUB $AA,$A2,$64,$41,$11,$99,$99,$91
        EQUB $55,$44,$44,$44,$44,$46,$62,$22
        EQUB $AA,$24,$44,$44,$45,$11,$11,$00

        ;EQUB $14,$15,$16,$15
        EQUS "4444"             ; Use Beebs character set
        EQUB $33,$8A,$8A,$8A
        EQUB $8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A
        EQUB $8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A
        EQUB $8A,$89,$11,$11,$11,$11,$19,$88
        EQUB $88,$88,$82,$22,$26,$44,$11,$00

        ;EQUB $17,$18,$19,$18
        EQUS "5555"             ; Use Beebs character set
        EQUB $33,$88,$88,$8A
        EQUB $88,$A8,$AA,$22,$22,$22,$22,$26
        EQUB $66,$66,$66,$44,$44,$51,$99,$99
        EQUB $98,$88,$89,$15,$44,$44,$44,$45
        EQUB $55,$55,$55,$54,$66,$66,$66,$00

; Following should load at $1C00
ORG $1C00
INCLUDE "Fonts.asm"

;----------------------------------------------------------------------------
; Beebs Relocation code goes here, needs to be on a page boundary 
; to make it easier to not relocate the relocator code
;----------------------------------------------------------------------------
ALIGN &100
.RELOC_START
    LDA #$8C
    LDX #$00
    LDY #$00
    JSR OSBYTE      ; Select cassette file system AND set speed

    LDX #0
    LDA #$0B
    JSR OSBYTE      ; Turn off Auto Repeat vsync_delay ?

    SEI             ; Disable interupts
    LDX #$FF        ; Initialise the stack
    TXS

    LDX #HI(RELOC_START-START)
	LDY #0

.relocloop
	LDA RELOAD_ADDR,Y
	STA NATIVE_ADDR,Y
	INY
	BNE relocloop
	INC relocloop+OFFSET+2		; PATCHED ADDRESS
	INC relocloop+OFFSET+5		; PATCHED ADDRESS
	DEX
	BNE relocloop

    ; Switch to mode 7 and initialise
    LDY #0
.init_loop
    LDA setup_screen + OFFSET,Y
    JSR OSWRCH
    INY
    CPY #12
    BNE init_loop

    LDA #%01111111 : STA sysVIADataDirectionRegA    ; Set keyboard data direction
    LDA #%00000011 : STA sysVIAPortB                ; keyboard write enable

    CLI                                             ; enable interupts
    JMP START

.setup_screen
EQUB 22,7                                           ; Mode 7
EQUB 23,1,0,0,0,0,0,0,0,0                           ; Turn off cursor

.RELOC_END

\ ******************************************************************
\ *	Save the code
\ ******************************************************************

PRINT
PRINT "-------------------------------------------------------------------"
PRINT "Abductor Memory Map"
PRINT "-------------------------------------------------------------------"
PRINT "Game loads at     : ", ~RELOAD_ADDR 
PRINT "Game relocates to : ", ~NATIVE_ADDR
PRINT "-------------------------------------------------------------------"
PRINT "End of ZP   : ", ~endZP
PRINT "-------------------------------------------------------------------"
PRINT "Bytes Used  : ", ~(RELOC_END-NATIVE_ADDR)
PRINT "Bytes Free  : ", ~(SCREEN_ADDRESS - RELOC_START)
PRINT "-------------------------------------------------------------------"
PRINT

SAVE "ABDUCTR", START, RELOC_END, RELOC_START+OFFSET, RELOAD_ADDR
