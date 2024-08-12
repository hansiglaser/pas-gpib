/*
 * Simple I2C grammar definition
 *
 * Tokens (see serial-i2c-lex.l for their regex):
 *  - each I2C event is one token
 *  - one token type per I2C event type
 *  - each token holds the timestamp and the optional data
 */

%{

// Pascal Declarations

Type 
  /// data type to hold information per token
  TI2CEvent = record
    Timestamp : Double;
    Data      : Byte;
  End;

/// debug write
Procedure DbgWr(St:String);
Begin
//  WriteLn(St);
End;

%}

/* YACC declarations */

%token <TI2CEvent>TOK_INIT_IN
%token <TI2CEvent>TOK_INIT_OUT
%token <TI2CEvent>TOK_FINAL_IN
%token <TI2CEvent>TOK_FINAL_OUT
%token <TI2CEvent>TOK_UNKNOWN
%token <TI2CEvent>TOK_ALIASED
%token <TI2CEvent>TOK_START
%token <TI2CEvent>TOK_STOP
%token <TI2CEvent>TOK_ADDR_WR_ACK
%token <TI2CEvent>TOK_ADDR_WR_NACK
%token <TI2CEvent>TOK_ADDR_RD_ACK
%token <TI2CEvent>TOK_ADDR_RD_NACK
%token <TI2CEvent>TOK_DATA_ACK
%token <TI2CEvent>TOK_DATA_NACK

%%

/* Grammar */

/* complete recording */
input:
       init {DbgWr('input init');} i2cxfers {DbgWr('input i2cxfers');} final {DbgWr('input final');};

init:
       TOK_INIT_OUT {DbgWr('init-a INIT_OUT');} |
       TOK_INIT_IN  {DbgWr('init-b INIT_IN');} tail {DbgWr('init-b tail');} TOK_STOP {DbgWr('init-b STOP');};

final:
       head          {DbgWr('final-a head');} TOK_FINAL_IN {DbgWr('final-a FINAL_IN');} |
       TOK_STOP      {DbgWr('final-b stop');} TOK_FINAL_IN {DbgWr('final-b FINAL_IN');} |
       TOK_FINAL_OUT {DbgWr('final-c final-out');};

head:
       wr_ack_head i2cdata |
       rd_ack_head i2cdata ;

tail:
       wr_ack | wr_ack_tail | rd_ack | rd_ack_tail;

i2cxfers:
       i2cxfers {DbgWr('i2cxfers i2cxfers');} i2cxfer {DbgWr('i2cxfers i2cxfer');} | /* empty */ ;

i2cxfer:
       TOK_UNKNOWN {DbgWr('i2cxfer-a UNKNOWN');} |
       TOK_ALIASED {DbgWr('i2cxfer-a ALIASED');} |
/*       TOK_START   {DbgWr('i2cxfer-b START');} |
       TOK_START   {DbgWr('i2cxfer-c START');} i2ccomms {DbgWr('i2cxfer-c i2ccomms');} |*/
       TOK_START   {DbgWr('i2cxfer-d START');} i2ccomms {DbgWr('i2cxfer-d i2ccomms');} TOK_STOP {DbgWr('i2cxfer-d STOP');};

i2ccomms:
       i2ccomms {DbgWr('i2ccomms i2ccomms');} i2ccomm {DbgWr('i2ccomms i2ccomm');} | /* empty */ ;

i2ccomm:
       wr_ack  {DbgWr('i2ccomm-a wr_ack');}  |
       wr_nack {DbgWr('i2ccomm-b wr_nack');} |
       rd_ack  {DbgWr('i2ccomm-c rd_ack');}  |
       rd_nack {DbgWr('i2ccomm-d rd_nack');};

wr_ack:
       wr_ack_head wr_ack_tail;

wr_ack_head:
       TOK_ADDR_WR_ACK  { Write  ($1.Timestamp:10:3,' Write to  ',IntToHex($1.Data,2),':');};

wr_ack_tail:
       i2cdata { WriteLn; };

wr_nack:
       TOK_ADDR_WR_NACK { WriteLn($1.Timestamp:10:3,' Write to  ',IntToHex($1.Data,2),' with NACK'); };

rd_ack:
       rd_ack_head { DbgWr('rd_ack-a rd_ack_head'); } |
       rd_ack_head { DbgWr('rd_ack-a rd_ack_head'); } rd_ack_tail {DbgWr('rd_ack-a rd_ack_tail');};

rd_ack_head:
       TOK_ADDR_RD_ACK  { Write  ($1.Timestamp:10:3,' Read from ',IntToHex($1.Data,2),':');};

rd_ack_tail:
       i2cdata TOK_DATA_NACK { WriteLn(' ',IntToHex($2.Data,2)); };

rd_nack:
       TOK_ADDR_RD_NACK { WriteLn($1.Timestamp:10:3,' Read from ',IntToHex($1.Data,2),' with NACK'); };


i2cdata:
       i2cdata TOK_DATA_ACK { Write(' ',IntToHex($2.Data,2)); } | {DbgWr('empty');}/* empty */;

/*
i2cevents:
         i2cevents i2cevent { DbgWr('i2cevents a'); }  | { DbgWr('i2cevents b'); };

anyi2cevent:
          i2cevent {
          DbgWr('i2cevent: ');
        };

i2cevent: 
        TOK_INIT_IN       { DbgWr($1.Timestamp); } |
        TOK_FINAL_OUT     { DbgWr($1.Timestamp); } |
        TOK_START          |
        TOK_STOP           |
        TOK_ADDR_WR_ACK    |
        TOK_ADDR_WR_NACK   |
        TOK_ADDR_RD_ACK    |
        TOK_ADDR_RD_NACK   |
        TOK_DATA_ACK       |
        TOK_DATA_NACK ;
*/

%%

// Additional code

