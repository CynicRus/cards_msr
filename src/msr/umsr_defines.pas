unit umsr_defines;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  // ESC is frequently used as a start delimiter character
  MSR_ESC = $1b;//Escape character

  // ASCII file separator character is used to separate track data
  MSR_FS = $1c; //File separator
  MSR_STS_OK = $30; // Ok
  MSR_STS_ERR = $41; //General error

  //Read/write commands

  MSR_CMD_READ = $72;//Formatted read
  MSR_CMD_WRITE = $77;//Formatted write

  MSR_CMD_RAW_READ = $6D;//raw read
  MSR_CMD_RAW_WRITE = $6E;//raw write

  //Status byte values from read/write commands
  MSR_STS_RW_ERR = $31;  //Read/write error */
  MSR_STS_RW_CMDFMT_ERR = $32;  // Command format error */
  MSR_STS_RW_CMDBAD_ERR = $34;  // Invalid command */
  MSR_STS_RW_SWIPEBAD_ERR = $39;  // Invalud card swipe in write mode */

  {
 * Read/write start and end delimiters.
 * The empty delimiter occurs when reading a track with no data.
 }

  MSR_RW_START = $73;  {* 's' *}
  MSR_RW_END = $3F;  {* '?' *}
  MSR_RW_BAD = $2A;  {* '*' *}
  MSR_RW_EMPTY = $2B;  {* '+' *}

(*
 * Serial port communications test
 * If serial communications are working properly, the device
 * should respond with a 'y' command.
 *)

  MSR_CMD_DIAG_COMM = $65;  (* Communications test *)
  MSR_STS_COMM_OK = $79;

(*
 * Sensor diagnostic command. Will respond with MSR_STS_OK once
 * a card swipe is detected. Can be interrupted by a reset.
 *)

  MSR_CMD_DIAG_SENSOR = $86;  (* Card sensor test *)
  MSR_STS_SENSOR_OK = MSR_STS_OK;

(*
 * RAM diagnostic command. Will return MSR_STS_OK if RAM checks
 * good, otherwise MSR_STS_ERR.
 *)

  MSR_CMD_DIAG_RAM = $87;  (* RAM test *)
  MSR_STS_RAM_OK = MSR_STS_OK;
  MSR_STS_RAM_ERR = MSR_STS_ERR;

(*
 * Set leading zero count. Responds with MSR_STS_OK if values
 * set ok, otherwise MSR_STS_ERR
 *)

  MSR_CMD_SLZ = $7A;  (* Set leading zeros *)
  MSR_STS_SLZ_OK = MSR_STS_OK;
  MSR_STS_SLZ_ERR = MSR_STS_ERR;

(*
 * Get leading zero count. Returns leading zero counts for
 * track 1/3 and 2.
 *)

  MSR_CMD_CLZ = $6C;  (* Check leading zeros *)

  (*
 * Erase card tracks. Returns MSR_STS_OK on success or
 * MSR_STS_ERR.
 *)

  MSR_CMD_ERASE = $63;  (* Erase card tracks *)
  MSR_STS_ERASE_OK = MSR_STS_OK;
  MSR_STS_ERASE_ERR = MSR_STS_ERR;

  MSR_ERASE_TK1 = $00;
  MSR_ERASE_TK2 = $02;
  MSR_ERASE_TK3 = $04;
  MSR_ERASE_TK1_TK2 = $03;
  MSR_ERASE_TK1_TK3 = $05;
  MSR_ERASE_TK2_TK3 = $06;
  MSR_ERASE_ALL = $07;

(*
 * Set bits per inch. Returns MSR_STS_OK on success or
 * MSR_STS_ERR.
 *)

  MSR_CMD_SETBPI = $62;  (* Set bits per inch *)
  MSR_STS_BPI_OK = MSR_STS_OK;
  MSR_STS_BPI_ERR = MSR_STS_ERR;

(*
 * Get device model number. Returns a value indicating a model
 * number, plus an 'S'.
 *)

  MSR_CMD_MODEL = $74;  (* Read model *)
  MSR_STS_MODEL_OK = $53;

  MSR_MODEL_MSR206_1 = $31;
  MSR_MODEL_MSR206_2 = $32;
  MSR_MODEL_MSR206_3 = $33;
  MSR_MODEL_MSR206_5 = $35;

  MSR_CMD_FWREV = $76;  (* Read firmware revision *)
  //MSR_FWREV_FMT    "REV?X.XX"

  (*
   * Set bits per character. Returns MSR_STS_OK on success, accompanied
   * by resulting per-track BPC settings.
   *)

  MSR_CMD_SETBPC = $6F;  (* Set bits per character *)
  MSR_STS_BPC_OK = MSR_STS_OK;
  MSR_STS_BPC_ERR = MSR_STS_ERR;

 (*
 * Set coercivity high or low. Returns MSR_STS_OK on success.
 *)

  MSR_CMD_SETCO_HI = $78;  (* Set coercivity high *)
  MSR_CMD_SETCO_LO = $79;  (* Set coercivity low *)
  MSR_STS_CO_OK = MSR_STS_OK;
  MSR_STS_CO_ERR = MSR_STS_ERR;

(*
 * Get coercivity. Returns 'H' for high coercivity, 'L' for low.
 *)

  MSR_CMD_GETGO = $64;  (* Read coercivity setting *)
  MSR_CO_HI = $48;
  MSR_CO_LO = $4C;

  (* The following commands have no response codes *)

  MSR_CMD_RESET = $61;  (* Reset device *)
  MSR_CMD_LED_OFF = $81;  (* All LEDs off *)
  MSR_CMD_LED_ON = $82;  (* All LEDs on *)
  MSR_CMD_LED_GRN_ON = $83;  (* Green LED on *)
  MSR_CMD_LED_YLW_ON = $84;  (* Yellow LED on *)
  MSR_CMD_LED_RED_ON = $85;  (* Red LED on *)

type
  TMSRCmd = record
    ESC: byte;
    CMD: byte;
  end;

  TMSREnd = record
    enddelim: byte;
    fs: byte;
    esc: byte;
    sts: byte;
  end;

  TMSRLz = record
    ESC: byte;
    lz_tk1_3: byte;
    lz_tk2: byte;
  end;

  TMSRModel = record
    ESC: Byte;
    Model: byte;
    S: byte;
  end;

implementation

end.

