TITLE How Low Can You Go    (howLowCanYouGo.asm)

; Author: Andrew Thiele
; Last Modified: 6/5/2019


COMMENT !

This program will:
	-Greet the user

	-Display program information to the user

	-Get 10 integers from the user and store the values in an array
	
	-Displays the integers, their sum and their average.

	-Give the user a farewell message
	
	-Implement user defined ReadVal and WriteVal procdures for UNSIGNED integers
		-Use implement getString and displayString macros
			-may use Irvine's ReadString for input and WriteString for output


Requirements:
	-Only ReadString and WriteString maybe used from Irvine library
!

INCLUDE Irvine32.inc

;code for macro is borrowed from demo7.asm written by Professor (Paul)2 son

;--------------------------------------------------------------
; getString
;
;Receives: offset of a string variable (name of variable)
;Description: uses names of string and variables to pass offset
;			  of string to EDX and offset of string size to EAX
;			  to pass length to ECX for IRVINE ReadString 
;			  procedure
;Registers used:  EAX, ECX, EDX
;Registers are saved and restored using system stack
;--------------------------------------------------------------
getString	MACRO	strOff, varOff
	push		EAX
	push		ECX
	push		EDX
	mov			EDX, strOff
	mov			ECX, 12
	call		ReadString
	mov			varOFF, EAX
	pop			EDX
	pop			ECX
	pop			EAX
				
ENDM


;--------------------------------------------------------------
;displayString
;
;Receives: string name
;Description: uses string name to pass offset to EDX register 
;			  for IRVINE WriteString procedure.  
;Registers used: EDX
;Registers are saved and restored using the system stack
;--------------------------------------------------------------

displayString MACRO buffer
	push	EDX
	mov		EDX, OFFSET buffer
	call	WriteString
	pop 	EDX
ENDM

; (insert constant definitions here)

.data
; (insert variable definitions here)
emptyStr				 BYTE           12 DUP(?) ;to convert from string to UNSIGNED integer

progTitle          BYTE           "How Low Can You Go", 0dh, 0ah, 0
titleAssist        BYTE           "Programmed by ", 0
authorName         BYTE           "Andrew Thiele", 0dh, 0ah, 0dh, 0ah, 0
progDescript1			 BYTE			"This program has the user enter 10 numbers.", 0dh, 0ah, 0
progDescript2			 BYTE			"The numbers are stored in an array.", 0dh, 0ah, 0
progDescript3			 BYTE			"They are then displayed along with their sum and average.", 0dh, 0ah, 0dh, 0ah, 0
instruction1       BYTE           "Enter a non-negative integer", 0dh, 0ah, 0
errorMsg1          BYTE           "NOT A NON-NEGATIVE INTEGER.  TRY AGAIN!!", 0dh, 0ah, 0
overflow				   BYTE			"Integer overflow.  TRY AGAIN!!", 0dh, 0ah, 0
comma	           	 BYTE           ",  ", 0
intsMsg			 		   BYTE			"The numbers in the array are: ", 0dh, 0ah, 0
sumMsg			 		   BYTE			"The sum of the numbers in the array is: ", 0dh, 0ah, 0
avgMsg			 		   BYTE			"The average of the numbers in the array is ", 0dh, 0ah, 0
partingMsg1        BYTE           "The program has finished.", 0dh, 0ah, 0dh, 0ah,  0  
partingMsg2        BYTE           "Since, this is the last assignment ...", 0dh, 0ah, 0dh, 0ah, 0  
goodBye            BYTE           "Thanks for grading my programs.", 0dh, 0ah, 0dh, 0ah, 0

array					     DWORD			10 DUP(?)

emptyStrSize			 DWORD			?; store size of emptyStr entry
tempInt			 		   DWORD			?; storage for number converted from string.  
sum						     DWORD			?
average					   DWORD			?


.code
main PROC
; (insert executable instructions here)

;display intro
				call 		Intro

;display program information
				call 		Information

;get 10 validated integers from user
				push		OFFSET array
				push		LENGTHOF array
				push		OFFSET tempInt
				call		FillArray
				call		CrLf
;display integers
				push		OFFSET array
				push		LENGTHOF array
				call		DisplayArray
				call		CrLf
;display sum
				push		OFFSET array
				push		LENGTHOF array
				push		OFFSET sum
				call		ArraySum
				call		CrLf
;display average

				push		LENGTHOF array
				push		sum
				push		OFFSET average
				call		ArrayAvg
				call		CrLf

;display outro
				call 		Outro

	exit	; exit to operating system
main ENDP

; (insert additional procedures here)

;--------------------------------------------------------------
;Intro
;
;Description: Displays the program title, and author name to 
;			  the user
;Calls: WriteString from displayString macro
;Receives: Macro receives offset of string into EDX
;Returns:  Text to console
;Preconditons: displayString macro must be included with procedure
;Registers Changed: EDX
;All registers are saved and restored using system stack
;--------------------------------------------------------------

Intro PROC
					displayString 		progTitle
					displayString 		titleAssist
					displayString 		authorName
					ret

Intro ENDP

;--------------------------------------------------------------
;Information
;
;Description: Displays program functionality to the user.
;
;Calls: WriteString from displayString macro
;Receives:  Macro receives offset of string into EDX
;Returns:   text to console
;Preconditons: displayString macro must be included with procedure
;Registers Changed: EDX
;All registers are saved and restored using system stack
;--------------------------------------------------------------

Information PROC
					displayString 		progDescript1
					displayString 		progDescript2
					displayString 		progDescript3
					ret

Information ENDP


;--------------------------------------------------------------
;ReadVal
;
;Description: Converts numberic string input into unsigned 
;		      integers
;Calls: getString macro
;Receives: Offset of integer, Offset of string, Offset of string 
;		   size
;Returns:  unsigned integer into DWORD variable
;Preconditons: push Offset of integer, Offset of string, Offset 
;			   of string size on to the system stack
;Registers Changed: EAX, EBX, ECX,EDI
;All registers are saved and restored using system stack
;--------------------------------------------------------------

ReadVal PROC
					push				EBP
					mov					EBP, ESP
					pushad
					cld
					mov					EDI, [EBP + 16] ; initialize integer storage to 0
					mov					EBX, 10

START:
					clc
					displayString		instruction1
					getString 			[EBP + 12], [EBP + 8]
					mov					ECX, [EBP + 8]			;number string size
					mov					ESI, [EBP + 12]			;number string offsett
					mov					EAX, 0
					mov					[EDI], EAX
					
					
CHECK_IF_INTEGER:
					mov					EAX, [EDI]
					mul					EBX
					jc					HAS_OVERFLOW
					mov					[EDI], EAX
					mov					EAX, 0
					lodsb
					cmp					AL, 30h
					jb					NOT_INTEGER
					cmp					AL, 39h
					ja					NOT_INTEGER					
					jmp					IS_INTEGER
NOT_INTEGER:				
					displayString		errorMsg1
					jmp					START					
HAS_OVERFLOW:	
					displayString		overflow
					jmp					START

IS_INTEGER:
					sub					AL, 30h
					
					add					[EDI], EAX
					loop				CHECK_IF_INTEGER
					popad
					pop					EBP
					ret					12
ReadVal ENDP


;--------------------------------------------------------------
;WriteVal
;
;Description: Displays unsigned integer to console
;
;Calls: WriteString from displayString Macro
;Receives: offset of a string, 32 bit unsigned integer 
;Returns: value in tempInt to the console as a string
;Preconditons: push tempInt and offset of empty string onto the 
;			   stack
;Registers Changed: EAX, EBX, EDX, EDI
;All registers are saved and restored using system stack
;--------------------------------------------------------------

WriteVal PROC
					push				EBP
					mov					EBP, ESP
					pushad	
					cld
					mov					EDI, [EBP + 12] ; OFFSET of emptyString
					mov					EAX, [EBP + 8] ; = OFFSET of tempInt
					mov					EBX, 10
					push				0 ; null terminator for string
CONVERTING_TO_CHAR:
					cdq
					div					EBX
					add					EDX, 30h
					push				EDX	

;Check if at the end of the number because quotient is in EAX				
					cmp					EAX, 0
					jne					CONVERTING_TO_CHAR
FILLING_STRING:
					pop					EAX
					stosb
					cmp					AL, 0
					jne					FILLING_STRING
					
					displayString		emptyStr
					popad				
					pop					EBP
					ret 8

WriteVal ENDP



;--------------------------------------------------------------
;FillArray
;
;Description: Fills an array with 10 unsigned integers
;
;Calls: ReadVal
;Receives: offset of array, size of array, offset of DWORD variable
;Returns: an array filled with 10 unsigned integers
;Preconditons:  push offset of array, length of array and tempInt
;				onto the system stack
;Registers Changed: EAX, ECX, EDX, EDI 
;All registers are saved and restored using system stack
;--------------------------------------------------------------

FillArray PROC		
					push 			EBP
					mov				EBP, ESP
					pushad
					mov				EDI, [EBP + 16]	;array offset
					mov				ECX, [EBP + 12]	;size of array
FILLING_ARRAY:
					
					push			OFFSET tempInt
					push			OFFSET emptyStr
					push			OFFSET emptyStrSize
					call			ReadVal
					mov				EDX, [EBP + 8]; tempInt converted from string
					mov				EAX, [EDX]
					stosd
				
					loop			FILLING_ARRAY
						
					popad
					pop				EBP
					ret				8

FillArray ENDP


;--------------------------------------------------------------
;Display Array
;
;Description: Displays the contents of an array
;
;Calls: WriteVal
;Receives: offset to an array, and array size on stack
;Returns: values inside an array
;Preconditons: array has integer values
;Registers Changed: EAX, ECX, EDX
;All registers are saved and restored using system stack
;--------------------------------------------------------------
DisplayArray PROC
					displayString 		intsMsg
					push				EBP
					mov					EBP, ESP
					pushad
					mov					ECX, [EBP + 8]; array size
					mov					ESI, [EBP + 12]; array offset
LOADING:
					lodsd
					push				OFFSET emptyStr
					push				EAX
					call				WriteVal
					cmp					ECX, 1
					je					NO_COMMA
					displayString		comma
NO_COMMA:
					loop				LOADING
					call				CrLf
					popad
					pop					EBP
					ret					8

DisplayArray ENDP


;--------------------------------------------------------------
;ArraySum
;
;Description:  Calculate the sum of an array
;
;Calls: WriteVal, and WriteString from displayString Macro
;Receives: Offset to array, offset of sum 
;Returns: the sum of numbers in an array in the variable in sum
;Preconditons: array is filled with integers
;Registers Changed: EAX, EBX, ECX, EDX, ESI
;All registers are saved and restored using system stack
;--------------------------------------------------------------
ArraySum PROC
					
					push				EBP
					mov					EBP, ESP
					pushad
					mov					ESI, [EBP + 16];array offset
					mov					ECX, [EBP + 12];array size
					mov					EBX, [EBP + 8];offset sum
MAKING_SUM:			
					lodsd
					add					[EBX], EAX
					loop				MAKING_SUM
					displayString 		sumMsg
					mov					EAX, [EBX]
					push				OFFSET emptyStr
					push				[EBX]
					call				WriteVal	;change to WriteVal
					call				CrLf
					
					popad
					pop					EBP
					ret 12
ArraySum ENDP


;--------------------------------------------------------------
;ArrayAvg
;
;Description:  Calculate the average of values in an array
;
;Calls: WriteString from displatString macro, WriteVal
;Receives: size of array, sum variable, offset of avg variable
;Returns: The average of an array in the variable avg
;Preconditons: An array filled with unsigned integers
;Registers Changed: EAX, EBX, ECX, EDX 
;All registers are saved and restored using system stack
;--------------------------------------------------------------
ArrayAvg PROC
					push				EBP
					mov					EBP, ESP
					pushad
					mov					ECX, [EBP + 8] ; offset of avg
					mov					EAX, [EBP + 12]; array sum
					mov					EBX, [EBP + 16]; size of array
					cdq			
					div					EBX
					mov					[ECX], EAX
					displayString 		avgMsg
					push				OFFSET emptyStr
					push				[ECX]
					call				WriteVal 
					call				CrLf
					popad
					pop					EBP
					ret 8
ArrayAvg ENDP


;--------------------------------------------------------------
;Outro
;
;Description: Displaying outro to user on the console
;
;Calls: writeString from displayString macro
;Receives: Offset of string to EDX 
;Returns: text to console
;Preconditons: displayString macro must be included with procedure
;Registers Changed: EDX
;All registers are saved and restored using system stack
;--------------------------------------------------------------

Outro PROC
					displayString 		partingMsg1
					displayString 		partingMsg2
					displayString 		goodBye
						
Outro ENDP


END main
