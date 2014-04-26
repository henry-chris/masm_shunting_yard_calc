;------------------------------------------------------------------------------
; Prog3_Henry.asm
; Christopher Henry
; CS308 Assignment 3
; E-mail: Henry.chris@gmail.com
;------------------------------------------------------------------------------
title Prog3_Henry.asm					;DOS file name of program

.586                                    ;enable all pentium instructions
.model flat, stdcall                    ;memory model & calling convention
.stack 8192                             ;allocate 8k for stack

INCLUDELIB kernel32.lib                 ;Include the kernel 32 library

;----------------------------------------------------------
; Constant Definitions
;----------------------------------------------------------

STD_INPUT  equ -10d                     ;Function number for keyboard input
STD_OUTPUT equ -11d                     ;Function number for monitor output

FALSE equ  0                            ;Flags
TRUE  equ  1                            ;

STD_INPUT  equ -10d                     ;Function number for keyboard input
STD_OUTPUT equ -11d                     ;Function number for monitor output

LF equ 10d                              ;Line feed ascii constant
CR equ 13d                              ;Carriage return constant
NEWLINE equ CR,LF                       ;Combine CR and LF for carriage return

C3 EQU 0100000000000000b                ;Condition codes which are checked
C2 EQU 0000010000000000b                ;  in the FPU Status register
C0 EQU 0000000100000000b                ;

ENABLE_PROCESSED_INPUT  equ 1           ;Flag to turn off line buffering
ENABLE_PROCESSED_OUTPUT equ 1           ;Flag to turn off line bufferin
ENABLE_LINE_WRAP        equ 3           ;Flag to trun line wrap on
DISABLE_PROCESSED_INPUT equ 7           ;Flag to turn on line buffering

CREATE_NEW    EQU  1                    ;Parameter for creating a new file
CREATE_ALWAYS EQU  2                    ;Always create (overwrite existing)
OPEN_EXISTING EQU  3                    ;Parameter for opening an existing file
GENERIC_READ  EQU  80000000h            ;Parameter for reading a file
GENERIC_WRITE EQU  40000000h            ;Parameter for writing a file

FILE_SHARE_READ   equ 1                 ;Parameter for reading a file
FILE_SHARE_WRITE  equ 2                 ;Parameter for writing to/creating a new file
FILE_SHARE_DELETE equ 4                 ;Parameter for deleting a file

FILE_ATTRIBUTE_NORMAL equ 80h           

HANDLE equ dword

;----------------------------------------------------------
; prototype Declarations for libarary imports
;----------------------------------------------------------
Sleep proto,							;Console sleeps for passed value in milliseconds
	dwMillisec:dword

ExitProcess proto,
    dwExitCode:dword				   ;The exit code for the process 

GetStdHandle proto, 
	nStdHandle: dword                  ;The standard device. -10=INPUT, -11=OUTPUT, -13=ERROR

SetConsoleMode proto,                  
    hConsoleHandle:dword,              ;A handle to the console input buffer or a console screen buffer
	dwMode:dword                       ;The input or output mode to be set. 

ReadFile proto,	
    hFile:dword,                       ;A handle to the device
	lpBuffer:near32,                   ;A pointer to the buffer that receives the data read 
    nNumberOfCharsToRead:dword,        ;The maximum number of bytes to be read.
    lpNumberOfbytesRead:near32,        ;A pointer to the variable that receives the number of bytes read
    lpOverlapped:near32                ;A pointer to an OVERLAPPED structure is required if the hFile parameter 
	                                   ;was opened with FILE_FLAG_OVERLAPPED, otherwise it can be NULL.

WriteFile proto,                  
    hFile:dword, lpBuffer:near32,      ;A handle to the device
    nNumberOfCharsToWrite:dword,       ;The maximum number of bytes to be written.
    lpNumberOfbytesWritten:near32,     ;A pointer to the variable that receives the number of bytes written
    lpOverlapped:near32                ;A pointer to an OVERLAPPED structure is required if the hFile parameter 
	                                   ;was opened with FILE_FLAG_OVERLAPPED, otherwise it can be NULL.


CreateFileA proto,                     ;Prototype for CreateFile, used for getting handle to new or existin file
    lpFileName:near32,
	dwDesiredAccess:dword,
	dwShareMode:dword,
	lpSecurityAttributes:near32,
	dwCreationDisposition:dword,
	dwFlagsAndAttributes:dword,
	hTemplateFile:dword

GetConsoleScreenBufferInfo proto,
	hConsole:dword,
	lpConsoleScreenBufferInfo:near32

FillConsoleOutputCharacterA proto,
	hConsoleOt:dword,
	cCharr:byte,
	nnLength:dword,
	dwwWriteCoord:dword,
	pNumberofCharsWritten:near32

FillConsoleOutputAttribute proto,
	hConsoleOutput:dword,
	wAttribute:word,
	nLength:dword,
	dwWriteCoord:dword,
	lpNumberOfAttrsWritten:near32

SetConsoleCursorPosition proto,
	hConOut:dword,
	dwCursorPos:dword

FlushConsoleInputBuffer proto,
	dwhConsoleIn:dword
	
GetLastError proto                     ;Prototype for getting specific error

OpIndex struct							; I was testing structures and decided to not use this for the postfix notation
	operator byte 0
	idx	     byte 0
OpIndex ends

;----------------------------------------------------------
; Data Segment -- Global Variables
;----------------------------------------------------------
.data
	strAddr			dd  ?						; Holds adress of instring
	strLength		dd  ?						; Holds length of null term string
	hStdOut			dd  ?						; Holds the out handle
	hStdIn			dd  ?						; Holds the In handle
	read			dd  ?						; Holds #characters read
	written			dd  ?						; Holds #chars written
	hFileIn			dd  ?						; Holds the Infile handle
	hFileOut		dd  ?						; Holds outfile Handle
	fill_screen     db 20h						; Holds how much console to fill in
	CCharsWritten      dd 0						; Holds #chars written
	size_screen_buffer dd 0						; Holds the size of the current screen buffer
	First_Cell         dw 0, 0					; Position of the top left of the console, the 'first'position	
	CONSOLE_SCREEN_BUFFER_INFO db 22 dup(0)		; GetConsoleScreenBufferInfo expects 22 bytes

	ansFP			dd 0						; Holds the FP answer
	ansStr			db 13 DUP(0)				; Holds the answer in IEEE format
	postStr			db 280 DUP(0)             	; Holds a decimal # in ascii format
	inputStr        db  140 DUP(0)				; Holds input strings from the user
    ovrflw          db  300 DUP(0)              ; Overflow string
	
	; These strings are output strings for the user menu and format
	menuStr1 db "Please enter a problem, or press [Enter] to exit. ", 
				 NEWLINE, NEWLINE , "You may only use +, -, *, /, [Space], and integer numbers: ", NEWLINE, NEWLINE, 0				
	menuStr2 db "Problem: ", NEWLINE, NEWLINE, 0
	menuStr3 db "Solution: ", NEWLINE, 0
	menuStr4 db "The problem you input is:  ", 0
	menuStr5 db "Answer:  ", 0
    menuStr6 db "==============================================================", NEWLINE, 0
	errorStr db "Error: Please input a valid problem.", NEWLINE, NEWLINE, 0
	exitStr   db NEWLINE, "Are you sure you want to exit?   y/n", NEWLINE, 0
	newlineStr db NEWLINE, NEWLINE, 0
	
;************ The following data items are required by fp2a and a2fp ********
  one      REAL4  1.0                   ;Used in fp2a
  ten      REAL4  10.0                  ;Used in a2fp and fp2a
  round    REAL4  0.000005              ;Used for rounding control in fp2a
  byteTen  BYTE   10                    ;Used in fp2a
  point    BYTE   ?                     ;Flag for a2fp
  minus    BYTE   ?                     ;Flag for a2fp
  digit    WORD   ?                     ;Used in a2fp and fp2a
  exponent WORD   ?                     ;Used in fp2a
  controlWd WORD  ?                     ;Used in fp2a

;----------------------------------------------------------
; Code Segment
;----------------------------------------------------------
.code
main proc
   xor eax, eax					; Start clearing all registers
   xor ebx, ebx
   xor ecx, ecx
   xor edx, edx					; End clearing all registers

   call Menu					; Call Menu proc and start the program

   mov ecx, 70                  ; Moves 70 into ecx
   lea edi, inputStr            ; Loads the used input string into edi
   cld                          ; Clears direction flag to scan string left to right
   rep stosw                    ; Stores NULL characters in inputStr to clear it

   mov ecx, 140                 ; Moves 140 into ecx
   lea edi, postStr             ; Loads the used postfix string into edi
   rep stosw                    ; Stores NULL characters in the postfix string to clear it
   
   jmp main                     ; Jumps back to main
main endp

;------------------------------------------------------------------------------------------
; InputError procedure
; Given: The user input string (inputStr), and the postfix string (postStr)
; Process:  The function will clear these variables of data, into bytes of NULL chars
;			This function preserves all flags and registers
; Returns:  Nothing
;-------------------------------------------------------------------------------------------
InputError proc
	pushad					        ; Push all registers
	pushfd					        ; Push all flags
	mov ax, 00h						; Move Null character to ax
	mov ecx, 70					    ; Put counter at 70
	
	lea edi, inputStr				; Get inputStr into edi for stosb
	cld								; Clear direction flag to go left to right
	rep stosw						; Clear the inputStr with Null characters
	
	mov ecx, 140					; Put counter at 140
	lea edi, postStr				; Point edi at postStr
	rep stosw						; Clear postStr with null chars

	call ClearScreen				; Clear the Screen
	lea esi, errorStr				; Load error string in esi
	call PrintString				; Print that there was an error
	invoke Sleep, 745				; Pause the console while the user reads

	popfd							; Pop all flags
	popad							; Pop all registers
	ret								; Return to caller
InputError endp

;---------------------------------------------------------------------------------------------
;Menu proc
;Given   :  Byte Strings printed with PrintString procedure.
;Process :	Prints the menu to the user, and gets the operation line.
;	        This procedure preserves all flags and registers.
;Returns :  Nothing
;---------------------------------------------------------------------------------------------
Menu proc
	pushad							; Push all registers
	pushfd							; Push all flags	
StartOver:
	xor eax, eax					; Start clearing all registers
    xor ebx, ebx
    xor ecx, ecx
    xor edx, edx					; End clearing all registers
	
	lea esi, menuStr1				; Load the menu string
	call PrintString				; Print the menu string

	lea esi, inputStr				; Load the input string
	call GetString					; Get the arithmetic expression
	mov eax, 131d					; Program will take up to 130 characters
	.if (eax <= read)				; If number of chars in the input is more than 130
		call InputError				; If it is, it's an error
		jmp StartOver				; Start the menu over if too many characters
	.endif							; End the .if directive

	call ClearScreen				; Clear the screen		

	lea esi, inputStr		        ; Load inputStr into esi
	call ParseInput				    ; This procedure checks for invalid input
	
	lea esi, inputStr				; Loads the input string into esi
	mov al, byte ptr [esi]			; Put the current char into al
	cmp al, 0						; If the string is empty after returning then start the menu over
	jz StartOver					; If the string is empty, start the menu over

	lea esi, inputStr				; Load inputStr into esi
	lea edi, postStr				; Load postStr into edi
	call InToPost					; This procedure Converts an infix arithmetic expression to postfix
	
	lea esi, postStr                ; Load the postfix destination string into esi
	call CalcAns					; This procedure will calculate the answer for the postfix expression

	fstsw ax						; Load fp flags in ax
	sahf							; Put flag bits into ah
	je StartOver					; If the zero flag is set, was error so start over

	lea esi, ansFP					; Load the FP answer into esi
	lea edi, ansStr					; Load the answer string into the destination pointer		
	call fp2a						; This procedure will convert the FP answer into an ascii string

	lea esi, menuStr4				; Load the menu string into esi
	call PrintString				; Print the string to the console

	lea esi, inputStr				; Load the input string into esi
	call PrintString				; Print the string to the console

	lea esi, newlineStr				; Load the newline string into esi
	call PrintString				; Print the string to the console

	lea esi, menuStr5				; Load the menu string into esi
	call PrintString				; Print the string to the console
		
	lea	esi, ansStr					; Load the answer string into esi
	call PrintString				; Print the answer string to the console

    lea esi, newlineStr             ; Load a formatting string into esi
    call PrintString                ; Print the newlineStr to the console

    lea esi, menuStr6               ; Load a formatting string into esi
    call PrintString                ; Print the menu string to the console

	lea esi, newlineStr				; Load the newline String into esi
	call PrintString				; Print the string to the console

	popfd							; Pop all the flags
	popad							; Pop all the registers
	ret								; Return to the caller
Menu endp

;--------------------------------------------------------------------------------------
;CalcAns procedure
;Given:   A string in ESI in postfix notation to calculate (postStr).
;Process: Takes the string in ESI, and calculates the postfix expression.
;		  This procedure does not affect any registers or flags
;Returns: Nothing
;--------------------------------------------------------------------------------------
CalcAns proc
	pushad			; Push all registers
	pushfd			; Push all flags

	xor ecx, ecx	; Clear ecx
	finit			; Initialize the FP stack to 0.0
	dec esi			; Decrement source pointer
NextByte:			; Get the next byte in the string
	inc esi			; Increment the source pointer
FindNum:			
	mov bl, byte ptr [esi]		; Move the current char into bl
	cmp bl, '0'					; If it's a number...
		jl NotNum				; Jump if not a number
PushFP:
	call a2fp				; Converts ascii number string to FP and pushes it to ST(0)
	jmp FindNum				; Get next char and compare
NotNum:						; If current char isn't a number
	cmp bl, ' '				; Is it a space?		
		je NextByte			; If so, ignore
	cmp bl, 0				; Is it a null char?
		je Done				; If so, we are done
	cmp bl, '+'				; Is it a plus sign?
		je Addition			; If it is, do addition
		jl Multiplication	; If a lower ascii char, must be multiplication
	cmp bl, '-'				; Is it a minus sign?
		je Subtraction		; If so, do subtraction
		jg Division			; If greater, must be division
IsError:
	call InputError			; If neither, program missed something, and it's an error
	jmp Done				; Get out if error
Division:					; Label called if division is needed
	ftst					;  Compare what's in st(0) to 0.0
	fstsw ax				; Store fp flags in ax
	sahf					; Put fp flags into EFLAGS
		je IsError			; Jump if there is an error
	fdiv					; Divide the top two numbers on the FP stack, stores in ST(0)
	jmp NextByte			; Jump to get the next byte
Multiplication:				; Label called if Multiplication is needed
	fmul					; Multiply the top two numbers on the FP stack, stores in ST(0)
	jmp NextByte			; Jump to get the next byte
Addition:					; Label called if Addition is needed
	fadd					; Add the top two numbers on the FP stack, stores in ST(0)
	jmp NextByte			; Jump to get the next byte
Subtraction:				; Label called if Subtraction is needed
	fsub					; Subtract the top two numbers on the FP stack, stores in ST(0)
	jmp NextByte			; Jump to get the next byte
Done:						; Label called if we reach the end of string in ESI
	fst ansFP				; Store the answer
	popfd					; Pop flags
	popad					; Pop registers
	ret						; Return to caller
CalcAns endp

;---------------------------------------------------------------------------------------
;ParseInput procedure
;Given: User input string containing the problem
;Process:  Checks for error conditions in the user input string. 
;Returns:  Nothing																
;---------------------------------------------------------------------------------------
ParseInput proc
	pushad							; Push all registers			
	pushfd							; Push all flags
	mov ecx, read                   ; Move the length of inputStr into ecx.  Must account for the previous CR and LF inn the string.
	cmp byte ptr [esi], 0			; Compare the current char to null.
		je ExitProg					; If the byte ptr is null, the string is empty and the user wants to exit.
	dec esi							; Dec esi so we can inc esi to begin the loop.  
StrCheck:
	inc esi							; Increment pointer to next byte
	mov al, byte ptr [esi]          ; Moves string byte into al.  Starts at beginning
	cmp al, '9'						; Compares byte to ascii '9'
		jg InError					; If above '9', it's an error
	cmp al, '/'						; The front slash is the last ascii character connected to the 0-9 series.
		jl IsLess					; If al is less than hex value of '/', then jump to IsLess
NextByte:
	call ChkMultiple				; Checks for multiple operators next to each other, which is an error condition
	mov dl, 1						; dl is one if last character was not a space
	mov ah, al						; Puts the old byte into ah.
LpCheck:
	loop StrCheck					; Checks ecx for 0, if not continue looping...ecx holds the inputStr length
	jmp EndLoop					    ; Jumps to procedure to check for invalid number of operators
SkipMult:
	mov dl, 2						; dl is 2 if last character was a space
	jmp LpCheck						; Jump to iterate the loop
IsLess:								; Jumps here if less than '/'	
	cmp al, ' '						; Compares al with ascii space
		jl InError					; If less than ascii value of space, then it's an error
		je SkipMult			    	; If equal to space, char is ok and get next byte
	cmp al, '*'						; Compare with ascii '*'
		jl InError					; If less than '*', it's an error
		je NextByte					; If equal, we can get next byte
	cmp al, '+'						; Compare with ascii '+'
		je NextByte					; If equal, we can get next byte
	cmp al, '-'						; Compare with ascii '-'
		jl InError					; If less, it's an error
		je NextByte					; If equal, we can get next byte
	jmp InError						; If none of the others, it must be ascii '.'  (period) and is an error
ExitProg:
	invoke ExitProcess, 0			; Exit program
InError:
    call InputError					; Calls procedure for an invalid input problem
EndLoop:	
	call ChkMultiple				; Ah and al are equal at this point. If the last character is an operator, it will result in an error.
	popfd							; Pop all flags
	popad							; Pop all registers
	ret								; Return to caller
ParseInput endp

;-------------------------------------------------------------------------
; InToPost procedure
; Given:   Esi pointing to an infix arithmetic string, edi pointing to 
;          an empty string.
; Process: The procedure converts the infix string to postfix and stores
;          the postfix string in the string pointed at by edi. The postfix
;          string puts a space at the end of every number as a marker.
; Returns: Nothing
;-------------------------------------------------------------------------
InToPost proc
	pushad							; Push all registers
	pushfd							; Push all flags
	xor eax, eax					; Start clearing all registers
    xor ebx, ebx
    xor ecx, ecx
    xor edx, edx					; End clearing all registers

	mov byte ptr [edi], 200         ; Move 200 into the first byte of the postfix string
StartPost:
	lodsb							; Load the current byte in esi into al
	cmp eax, 0						; If we reach the null terminator, end
		je PopRestStack				; If the char is null, Pop the rest of the stack into the Post string

	.if (  (eax >= '0')  && (eax <= '9')  )  ; If it is a number, add to postStr
		 stosb								 ; Store al into the current edi byte, increment edi
		 jmp StartPost						 ; Jump back and compare next char

	.elseif ( eax == ' ' )					 ; Is the character a space?
		jmp StartPost						 ; Jump back and compare next char if so
	.endif									 ; End the .if directive
	
	; String is valid, so anything below this is a valid operator
	mov byte ptr [edi], ' '		 ; Move a space into the current post string byte
	inc edi						 ; Increment edi to the next byte
	cmp ecx, 0					 ; Compare ecx to 0
		jne NotEmpty			 ; Jump if stack is not empty
EmptyStack:						 ; Called if nothing currently on the stack		
	push eax					 ; Push the operation to the stack
	inc ecx						 ; Increment the stack counter
	jmp StartPost				 ; Jump back to get next char
NotEmpty:					     ; If stack not empty, compare precedence of char with topStack (edx)

	.if ( (byte ptr [esp] == '+') || (byte ptr [esp] == '-') )   ;If top of stack is '+' or '-', precedence is 1
		mov dl, 1				 ; Precedence is 1
	.else
		mov dl, 2				 ; Otherwise precedence is higher
	.endif						 ; End the .if directive
	
	.if ( (eax == '+') || (eax =='-') )   ; If current operator is '+' or '-', prec. is 1
		mov dh, 1				; Precedence is 1
	.else 
		mov dh, 2				; Otherwise prec. is higher
	.endif						; End the .if directive

	cmp dh, dl
		jle PopStack			; Topstack(dl) has higher priority
		jg PushScanned			; Topstack is lower priority
PopStack:						; Pops the stack into ebx and moves it into postStr
	pop ebx
	mov byte ptr [edi], bl		; Move bl into postStr
	inc edi						; Increment postStr pointer
	dec ecx						; Decrement the stack counter
	cmp ecx, 0					; Compare items on stack to 0
		jg NotEmpty				; If items on stack, jump to NotEmpty
PushScanned:					; else push the scanned operator onto stack
	push eax					; Push current operator onto stack
	inc ecx						; increment stack counter
	jmp StartPost				; Next character
PopRestStack: 					; At end of inputStr, push remaining operators into postStr
	mov eax, ' '				; Put an empty space in eax
	stosb						; Store the space in the string pointed at by edi(postStr), and increment edi
	cmp ecx, 0
		je Done					; If stack counter at 0, do not pop the stack
LoopIt:
	pop eax						; Pop stack into eax
	stosb						; Store eax in postStr
	loop LoopIt					; Loop the pop and store until nothing on stack
Done:
	popfd						; Pop all flags
	popad						; Pop all registers
	ret							; Return to caller
InToPost endp

;--------------------------------------------------------------------------------------------------
; ChkMultiple procedure
; Given:  Ah contains the last succussful character that was not a space. Al contains the current
;		  allowed character in the inputStr that is not a space.
; Process:  This function will check for multiple operation characters, (+ - / * ) in a row.
;           It will also check for a string starting with an operation, negatives are not allowed.
; Returns: Nothing
;---------------------------------------------------------------------------------------------------
ChkMultiple proc	
	cmp ah, 0				  ; Is the character null?
		je IsOperation		  ; If first run through, ah will be null and we will check for a beginning operator(error condition)
	cmp ah, '0'				  ; Is the character a number or operation?
		jge IsNumber     	  ; Last successful char is a number, and is a valid char.
IsOperation:				  ; If not a number, it's an operation
	cmp al, '0'				  ; Compare the current operator with '0'
		jge Done			  ; If <= then 2 operators next to each other...error
	call InputError			  ; Call error proc
    mov ecx, 1                ; Put the counter at 1 so loop stops
	jmp Done
IsNumber:
	.if(  (ah > '/') && (al > '/') && (dl == 2) )	; If current and last valid char are numbers, but there's a space between
		call InputError								; It is an error
		mov ecx, 1									; Put counter at 1 so loop stops
	.endif											; End the if directive
Done:
	ret						  ; Return to caller
ChkMultiple endp

;---------------------------------------------------------------------------------------------
;ClearScreen proc
;Given   :  Nothing
;Process :	Calculates the current size of the screen buffer width*height and fills the console
;           with the appropriate number of black spaces to effectively clear the screen.  It
;			then sets the cursor position to the top left for next use.
;	        This procedure preserves all flags and registers.
;Returns :  Nothing
;---------------------------------------------------------------------------------------------
ClearScreen proc						; Define procedure
	 pushad							    ; save registers
     pushfd								; save flags
	 invoke GetStdHandle, STD_OUTPUT
	 mov hStdOut, eax
	 invoke GetConsoleScreenBufferInfo, hStdOut,     ; Calculating width*height
		    near32 ptr CONSOLE_SCREEN_BUFFER_INFO
	
	 lea esi, CONSOLE_SCREEN_BUFFER_INFO
	 mov ax, word ptr [esi]
     mov bx, word ptr [esi+2]						 ;height
	 imul bx										 ; Getting the size of the current console screen buffer
	 mov size_screen_buffer, eax					 ; Putting the the size into size_screen_buffer
	 xor eax, eax									 ; Clearing eax for FillConsoleOutputCharacterA  ***
	 invoke FillConsoleOutputCharacterA, hStdOut, " ", size_screen_buffer,   ; Fills the current console buffer with black spaces
		    First_Cell, near32 ptr CCharsWritten							 ; Clears the console screen
	 
	 invoke SetConsoleCursorPosition, hStdOut, First_Cell					  ; Sets the consoles cursor to the 0,0 or top left position
	 popfd                      ; Restore flags
	 popad                      ; Restore registers
	 ret                        ; Return to caller
ClearScreen endp

;------------------------------------------------------------------------------
; Procedure to print a string to stdout;
; Given   :  The Address of Null (0) terminated String to print in ESI register
; process :  Print the String using the kernel32.lib WriteFile to
;         :  Standard_Output function call.  No registers are changed and the
;         :  flags are not affected.
; Return  :  Nothing
;------------------------------------------------------------------------------
PrintString proc                       ; Define procedure
            pushad                     ; save registers
            pushfd                     ; save flags
            mov    strAddr, esi        ; copy string address
                                       ; find string length
            mov    strLength, 0        ; initialize string length
WhileChar:  cmp    byte ptr [esi], 0   ; character = null?
            jz     EndWhileChar        ; exit if so
            inc    strLength           ; increment character count
            inc    esi                 ; point at next character
            jmp    WhileChar           ; while more characters exist
EndWhileChar:
            invoke GetStdHandle,STD_OUTPUT ; get handle for console output
            mov    hStdOut, eax        ; copy file handle for screen
            invoke WriteFile,          ; invoke standard WriteFile with
              hStdOut,                 ;   file handle for screen
              strAddr,                 ;   address of string
              strLength,               ;   length of string
              near32 ptr written,      ;   variable for # bytes written
              0                        ;   overlapped mode
            popfd                      ; restore flags
            popad                      ; restore registers
            ret                        ; return to caller
PrintString endp

;------------------------------------------------------------------------------
; Procedure to get a string from stdin;
; Given   :  The Address of the String to fill in ESI register
; process :  Input the String using the kernel32.lib ReadFile from the
;         :  Standard_Input function call.  No registers are changed and the
;         :  flags are not affected.
; Return  :  The input string in the data segment
;------------------------------------------------------------------------------
GetString proc                         ; Define procedure
            pushad                     ; save all registers
            pushfd                     ; save flags

            invoke GetStdHandle,STD_INPUT  ; get handle for console
            mov    hStdIn, eax         ; save the handle
            invoke SetConsoleMode,     ; invoke standard console with
              hStdIn,                  ;   file handle for keyboard
              DISABLE_PROCESSED_INPUT  ;   turn line buffering on

            mov    ecx,255d  ;MaxStr   ; string length
            mov    strLength, ecx      ; maximum string to accept
            mov    strAddr, esi        ; save pointer to input string
            invoke ReadFile,           ; invoke standard ReadFile with
              hStdIn,                  ;   file handle for keyboard
              strAddr,                 ;   address of string
              strLength,               ;   length of string
              near32 ptr read,         ;   variable for # bytes read
              0                        ;   overlapped mode
            mov ecx, read              ; number of bytes read
            mov byte ptr [esi+ecx-2],0 ; replace CR/LF by trailing null
			sub read, 2

            popfd                      ; restore flags
            popad                      ; restore registers
            ret                        ; return to caller
GetString   endp

;--------- Convert Floating Point Number to ASCII String ----------------------
; Given   :ESI pointing to FP value to convert and EDI pointing to the output 
;          string destination.
; Process :Build ASCII string with format [blank/-]d.dddddE[+/-]dd and store
;          the string in the data segment. (Output is always 12 characters.)
;          No registers are changed and the flags are not affected.
; Return  :Nothing
;------------------------------------------------------------------------------
fp2a PROC NEAR32
          pushad                ;Save the contents of all registers
          pushfd                ;
          finit                 ; initialize the Floating Point Stack
          fstcw controlWd       ; get control word
          push controlWd        ; save control word
          or   controlWd, 0000110000000000b
          fldcw controlWd       ; set control to chop
          fld REAL4 PTR [esi]   ; load ST with the FP value
          mov  exponent, 0      ; exponent := 0
          ftst                  ; value >= 0?
          fstsw  ax             ; status word to AX
          and  ax, C0           ; check C0
          jnz  elseNeg          ; skip if set (value negative)
          mov al,' '            ; blank for positive
          stosb                 ; write the blank to the string
          jmp  NotNeg           ; jump over negative sign
elseNeg:  mov al,'-'            ; minus for negative
          stosb                 ; write the minus to the string
          fchs                  ; convert number to positive
NotNeg:
          mov  exponent, 0      ; exponent := 0
          ftst                  ; value = 0?
          fstsw ax              ; status word to AX
          and  ax, C3           ; check C3
          jne  endifZero        ; skip if zero
          fcom ten              ; value > 10?
          fstsw ax              ; status word to AX
          and  ax, C3 or C2 or C0   ; check for all C3=C2=C0=0
          jnz  elseLess         ; skip if value not > 10
untilLess:
          inc  exponent         ; add 1 to exponent
          fdiv ten              ; value := value/10
          fcom ten              ; value < 10
          fstsw ax              ; status word to AX
          and  ax, C0           ; check C0
          jz  untilLess         ; continue until value < 10
          jmp  endifBigger      ; exit if
elseLess:
whileLess:
          fcom one              ; value < 1
          fstsw ax              ; status word to AX
          and  ax, C0           ; check C0
          jz   endwhileLess     ; exit if not less
          fmul ten              ; value := 10*value
          dec  exponent         ; subtract 1 from exponent
          jmp  whileLess        ; continue while value < 1
endwhileLess:
endifBigger:
endifZero:
          fadd round            ; add rounding value
          fcom ten              ; value > 10?
          fstsw ax              ; status word to AX
          and  ax, C3 or C2 or C0  ; C3=C2=C0=0? (value > 10?)
          jnz  endifOver        ; skip if not
          fdiv ten              ; value := value/10
          inc  exponent         ; add 1 to exponent
endifOver:
                                ; at this point 1.0 <= value < 10.0
          fist digit            ; store integer part
          mov  bx, digit        ; copy integer to BX
          or   bx, 30h          ; convert digit to character
          mov al,bl             ; copy character to AL for writing
          stosb                 ; write the character to the string
          mov al,'.'            ; load AL with decimal point
          stosb                 ; write decimal point to string

          mov ecx,5             ; count of remaining digits
forDigit: fisub digit           ; subtract integer part
          fmul ten              ; multiply by 10
          fist digit            ; store integer part
          mov  bx, digit        ; copy integer to BX
          or   bx, 30h          ; convert digit to character
          mov al,bl             ; copy character to write
          stosb                 ; write the character to the string
          loop forDigit         ; repeat 5 times
          mov al,'E'            ; exponent indicator
          stosb                 ; write the character to the string
          mov  ax, exponent     ; get exponent
          push ax               ; save the exponenet
          cmp  ax, 0            ; exponent >= 0 ?
          jnge NegExp
          mov al,'+'            ; non-negative exponent
          stosb                 ; write the character to the string
          pop ax                ; restore the exponent if not negative
          jmp  endifNegExp
NegExp:   mov al,'-'            ; negative exponent
          stosb                 ; write the character to the string
          pop ax                ; restore the exponent if negative
          neg ax                ; change exponent to positive
endifNegExp:
          div  byteTen          ; convert exponent to 2 digits
          or   ax, 3030h        ; convert both digits to ASCII
          stosb                 ; write AL character to the string
          mov al,ah             ; copy low order character to DL
          stosb                 ; write the character to the string
          xor al,al             ; zero the AL register
          stosb                 ; write NULL to terminate string
          pop  controlWd        ; restore control word
          fldcw controlWd       ; load the control word back to FPU
          popfd                 ;Restore the registers
          popad                 ;
          ret                   ;Return to calling procedure
fp2a  ENDP

;--------- Convert ASCII String to Floating Point Number ----------------------
; Given   :ESI register pointing to the ASCII number to convert
; Process :After an optional leading minus sign, only digits 0-9 and a decimal
;          point are accepted -- the scan terminates with any other character.
;          This procedure produces an IEEE floating point number.
;          The flags are not affected.
; Return  :The floating point value is returned in ST.
;------------------------------------------------------------------------------
a2fp PROC NEAR32
        ;  pushad               ;Save the contents of all registers
          pushfd               ;
         ; finit                ;Initialize the Floating Point Stack
          fld1                 ; divisor := 1.0
          fldz                 ; value := 0.0
          mov  point, false    ; no decimal point found yet
          mov  minus, false    ; no minus sign found yet
          lodsb                ; load first character into AL register
          cmp al,'-'           ; check for minus sign
          jne  NotMinus        ; skip if not
          mov  minus, true     ; minus sign found
          lodsb                ; load next character into AL register
NotMinus:                      ; was not negative number
WhileMore:cmp  al, '.'         ; decimal point?
          jne  NotPoint        ; skip if not
          mov  point, true     ; found decimal point
          jmp  nextChar
NotPoint: cmp  al, '0'         ; character a digit?
          jl   EndWhile        ; exit if lower than '0'
          cmp  al, '9'
          jg   EndWhile        ; exit if higher than '9'
          and  ax, 000fh       ; convert ASCII to integer value
          mov  digit, ax       ; put integer in memory
          fmul ten             ; value := value * 10
          fiadd digit          ; value := value + digit
          cmp  point, true     ; already found a decimal point?
          jne  endifDec        ; skip if not
          fxch                 ; put divisor in ST and value in ST(1)
          fmul ten             ; divisor := divisor * 10
          fxch                 ; value back to ST; divisor back to ST(1)
endifDec:
nextChar: lodsb                ; load next character into AL registe
          jmp  WhileMore       ; go process the character
EndWhile: fdivr                ; value := value / divisor
          cmp  minus, true     ; was there a minus sign?
          jne  IsPositive      ; no, it is a positive number
          fchs                 ; yes, value := -value
IsPositive:
          popfd                ;Restore the registers
         ; popad                ;
          ret                  ;Return to calling procedure
a2fp  ENDP

end  ; end directive to compiler