//  libtox.pas
//
//  Обертка для библиотеки libtox.
//
//  The MIT License (MIT)
//
//  Copyright (c) 2013 Dmitry
//
unit libtox;

interface
  {$I tox.inc}

uses
  {$I tox-uses.inc}
  {$IFDEF FPC}dynlibs,{$ENDIF}
  {$IFDEF UNIX}dl,{$ENDIF}
  SysUtils;

type
	TTox = Pointer;

//typedef union {
//    uint8_t c[4];
//    uint16_t s[2];
//    uint32_t i;
//} tox_IP;
  PToxIp = ^TToxIp;
  TToxIp = record
//    c: Integer;
//    s: Integer;
    i: Integer;
  end;

//typedef struct {
//    tox_IP ip;
//    uint16_t port;
//    /* Not used for anything right now. */
//    uint16_t padding;
//} tox_IP_Port;
  PToxIpPort = ^TToxIpPort;
  TToxIpPort = record
    ip: TToxIp;    
    port: Word;

    padding: Word;
  end;

//enum {
//    TOX_FAERR_TOOLONG = -1,
//    TOX_FAERR_NOMESSAGE = -2,
//    TOX_FAERR_OWNKEY = -3,
//    TOX_FAERR_ALREADYSENT = -4,
//    TOX_FAERR_UNKNOWN = -5,
//    TOX_FAERR_BADCHECKSUM = -6,
//    TOX_FAERR_SETNEWNOSPAM = -7,
//    TOX_FAERR_NOMEM = -8
//};

  TToxFaerr = (tfFriendNumber = 0, tfTooLong = -1, tfNoMessage = -2, tfOwnKey = -3,
    tfAlReadySend = -4, tfUnknown = -5, tfBadChecksum = -6, tfSetNewNospam = -7,
    tfNoMem = -8);

///* USERSTATUS -
// * Represents userstatuses someone can have.
// */
//typedef enum {
//    TOX_USERSTATUS_NONE,
//    TOX_USERSTATUS_AWAY,
//    TOX_USERSTATUS_BUSY,
//    TOX_USERSTATUS_INVALID
//}
//TOX_USERSTATUS;
  PToxUserStatus = ^TToxUserStatus;
  TToxUserStatus = (usNone, usAway, usBusy, usInvalid);

  TProcFriendRequest = procedure(public_key: PByte; data: PByte; length: Word; UserData: Pointer); cdecl;
  TProcFriendMessage = procedure(tox: TTox; FriendNumber: Integer; message: PByte; length: Word; UserData: Pointer); cdecl;
  TProcAction = procedure(tox: TTox; FriendNumber: Integer; Action: PByte; length: Word; UserData: Pointer); cdecl;
  TProcNameChange = procedure(tox: TTox; FriendNumber: Integer; NewName: PByte; length: Word; UserData: Pointer); cdecl;
  TProcStatusMessage = procedure(tox: TTox; FriendNumber: Integer; NewStatus: PByte; length: Word; UserData: Pointer); cdecl;
  TProcUserStatus = procedure(tox: TTox; FriendNumber: Integer; Kind: TToxUserStatus; UserData: Pointer); cdecl;
  TProcReadReceipt = procedure(tox: TTox; FriendNumber: Integer; Receipt: Integer; UserData: Pointer); cdecl;
  TProcConnectionStatus = procedure(tox: TTox; FriendNumber: Integer; Status: Byte; UserData: Pointer); cdecl;
  TProcGroupInvite = procedure(tox: TTox; FriendNumber: Integer; GroupPublicKey: PByte; UserData: Pointer); cdecl;

const
  TOX_MAX_NAME_LENGTH = 128;
  TOX_MAX_STATUSMESSAGE_LENGTH = 128;
  TOX_CLIENT_ID_SIZE = 32;
  FRIEND_ADDRESS_SIZE = (TOX_CLIENT_ID_SIZE + sizeof(Integer) + sizeof(Word));

  TOX_LIBRARY = {$IFDEF Win32}'libtoxcore-0.dll'{$ENDIF}
                {$IFDEF Unix}'libtoxcore-0.so'{$ENDIF};

///* Run this at startup.
// *
// *  return allocated instance of tox on success.
// *  return 0 if there are problems.
// */
//void *tox_new(uint8_t ipv6enabled)
function tox_new(ipv6enabled: Byte): TTox; cdecl; external TOX_LIBRARY;

///*  return FRIEND_ADDRESS_SIZE byte address to give to others.
// * format: [client_id (32 bytes)][nospam number (4 bytes)][checksum (2 bytes)]
// */
procedure tox_getaddress(tox: TTox; address: PByte);

///* Add a friend.
// * Set the data that will be sent along with friend request.
// * address is the address of the friend (returned by getaddress of the friend you wish to add) it must be FRIEND_ADDRESS_SIZE bytes. TODO: add checksum.
// * data is the data and length is the length.
// *
// *  return the friend number if success.
// *  return TOX_FA_TOOLONG if message length is too long.
// *  return TOX_FAERR_NOMESSAGE if no message (message length must be >= 1 byte).
// *  return TOX_FAERR_OWNKEY if user's own key.
// *  return TOX_FAERR_ALREADYSENT if friend request already sent or already a friend.
// *  return TOX_FAERR_UNKNOWN for unknown error.
// *  return TOX_FAERR_BADCHECKSUM if bad checksum in address.
// *  return TOX_FAERR_SETNEWNOSPAM if the friend was already there but the nospam was different.
// *  (the nospam for that friend was set to the new one).
// *  return TOX_FAERR_NOMEM if increasing the friend list size fails.
// */
function tox_addfriend(tox: TTox; address: PByte; data: PByte; length: Word): Integer;

///* Add a friend without sending a friendrequest.
// *  return the friend number if success.
// *  return -1 if failure.
// */
function tox_addfriend_norequest(tox: TTox; client_id: PByte): Integer;

///*  return the friend id associated to that client id.
//    return -1 if no such friend */
function tox_getfriend_id(tox: TTox; client_id: PAnsiChar): Integer;

///* Copies the public key associated to that friend id into client_id buffer.
// * Make sure that client_id is of size CLIENT_ID_SIZE.
// *  return 0 if success.
// *  return -1 if failure.
// */
//int tox_getclient_id(Tox *tox, int friend_id, uint8_t *client_id);
function tox_getclient_id(tox: TTox; friend_id: Integer; client_id: PByte): Integer;

///* Remove a friend. */
//int tox_delfriend(Tox *tox, int friendnumber);
function tox_delfriend(tox: TTox; friendnumber: Integer): Integer;

///* Send a text chat message to an online friend.
// *
// *  return the message id if packet was successfully put into the send queue.
// *  return 0 if it was not.
// *
// * You will want to retain the return value, it will be passed to your read receipt callback
// * if one is received.
// * m_sendmessage_withid will send a message with the id of your choosing,
// * however we can generate an id for you by calling plain m_sendmessage.
// */
//uint32_t tox_sendmessage(Tox *tox, int friendnumber, uint8_t *message, uint32_t length);
function tox_sendmessage(tox: TTox; friendnumber: Integer; message: PByte; length: Integer): Integer;

//uint32_t tox_sendmessage_withid(Tox *tox, int friendnumber, uint32_t theid, uint8_t *message, uint32_t length);
function tox_sendmessage_withid(tox: TTox; friendnumber: Integer; theid: Integer; message: PAnsiChar; length: Integer): Integer;

///* Send an action to an online friend.
// *
// *  return 1 if packet was successfully put into the send queue.
// *  return 0 if it was not.
// */
//int tox_sendaction(Tox *tox, int friendnumber, uint8_t *action, uint32_t length);
function tox_sendaction(tox: TTox; friendnumber: Integer; action: PAnsiChar; length: Integer): Integer;

///* Set our nickname.
// * name must be a string of maximum MAX_NAME_LENGTH length.
// * length must be at least 1 byte.
// * length is the length of name with the NULL terminator.
// *
// *  return 0 if success.
// *  return -1 if failure.
// */
//int tox_setname(Tox *tox, uint8_t *name, uint16_t length);
function tox_setname(tox: TTox; name: PByte; length: Word): Integer;

///*
// * Get your nickname.
// * m - The messanger context to use.
// * name - Pointer to a string for the name.
// * nlen - The length of the string buffer.
// *
// *  return length of name.
// *  return 0 on error.
// */
//uint16_t tox_getselfname(Tox *tox, uint8_t *name, uint16_t nlen);
function tox_getselfname(tox: TTox; name: PByte; nlen: Word): Word;

///* Get name of friendnumber and put it in name.
// * name needs to be a valid memory location with a size of at least MAX_NAME_LENGTH (128) bytes.
// *
// *  return length of name (with the NULL terminator) if success.
// *  return -1 if failure.
// */
//int tox_getname(Tox *tox, int friendnumber, uint8_t *name);
function tox_getname(tox: TTox; friendnumber: Integer; name: PByte): Integer;

///* Set our user status.
// * You are responsible for freeing status after.
// *
// *  returns 0 on success.
// *  returns -1 on failure.
// */
//int tox_set_statusmessage(Tox *tox, uint8_t *status, uint16_t length);
function tox_set_statusmessage(tox: TTox; status: PByte; length: Word): Integer;

//int tox_set_userstatus(Tox *tox, TOX_USERSTATUS status);
function tox_set_userstatus(tox: TTox; status: TToxUserStatus): Integer;

///*  return the length of friendnumber's status message, including null.
// *  Pass it into malloc
// */
//int tox_get_statusmessage_size(Tox *tox, int friendnumber);
function tox_get_statusmessage_size(tox: TTox; friendnumber: Integer): Integer;

///* Copy friendnumber's status message into buf, truncating if size is over maxlen.
// * Get the size you need to allocate from m_get_statusmessage_size.
// * The self variant will copy our own status message.
// */
//int tox_copy_statusmessage(Tox *tox, int friendnumber, uint8_t *buf, uint32_t maxlen);
function tox_copy_statusmessage(tox: TTox; friendnumber: Integer; buf: PByte; maxlen: Integer): Integer;

//int tox_copy_self_statusmessage(Tox *tox, uint8_t *buf, uint32_t maxlen);
function tox_copy_self_statusmessage(tox: TTox; buf: PByte; maxlen: Integer): Integer;

///*  return one of USERSTATUS values.
// *  Values unknown to your application should be represented as USERSTATUS_NONE.
// *  As above, the self variant will return our own USERSTATUS.
// *  If friendnumber is invalid, this shall return USERSTATUS_INVALID.
// */
//TOX_USERSTATUS tox_get_userstatus(Tox *tox, int friendnumber);
function tox_get_userstatus(tox: TTox; friendnumber: Integer): TToxUserStatus;

//TOX_USERSTATUS tox_get_selfuserstatus(Tox *tox);
function tox_get_selfuserstatus(tox: TTox): TToxUserStatus;

///* Sets whether we send read receipts for friendnumber.
// * This function is not lazy, and it will fail if yesno is not (0 or 1).
// */
//void tox_set_sends_receipts(Tox *tox, int friendnumber, int yesno);
procedure tox_set_sends_receipts(tox: TTox; friendnumber: Integer; yesno: Integer);

///* Allocate and return a list of valid friend id's. List must be freed by the
// * caller.
// *
// * retun 0 if success.
// * return -1 if failure.
// */
//int tox_get_friendlist(void *tox, int **out_list, uint32_t *out_list_length);
function tox_get_friendlist(tox: TTox; out out_list: PByte; out_list_length: PInteger): Integer;

///* Set the function that will be executed when a friend request is received.
// *  Function format is function(uint8_t * public_key, uint8_t * data, uint16_t length)
// */
//void tox_callback_friendrequest(Tox *tox, void (*function)(uint8_t *, uint8_t *, uint16_t, void *), void *userdata);
procedure tox_callback_friendrequest(tox: TTox; CallBack: TProcFriendRequest; UserData: Pointer);

///* Set the function that will be executed when a message from a friend is received.
// *  Function format is: function(int friendnumber, uint8_t * message, uint32_t length)
// */
//void tox_callback_friendmessage(Tox *tox, void (*function)(Tox *tox, int, uint8_t *, uint16_t, void *),
//                                void *userdata);
procedure tox_callback_friendmessage(tox: TTox; CallBack: TProcFriendMessage; UserData: Pointer);

///* Set the function that will be executed when an action from a friend is received.
// *  Function format is: function(int friendnumber, uint8_t * action, uint32_t length)
// */
//void tox_callback_action(Tox *tox, void (*function)(Tox *tox, int, uint8_t *, uint16_t, void *), void *userdata);
procedure tox_callback_action(tox: TTox; CallBack: TProcAction; UserData: Pointer);

///* Set the callback for name changes.
// *  function(int friendnumber, uint8_t *newname, uint16_t length)
// *  You are not responsible for freeing newname
// */
//void tox_callback_namechange(Tox *tox, void (*function)(Tox *tox, int, uint8_t *, uint16_t, void *),
//                             void *userdata);
procedure tox_callback_namechange(tox: TTox; CallBack: TProcNameChange; UserData: Pointer);

///* Set the callback for status message changes.
// *  function(int friendnumber, uint8_t *newstatus, uint16_t length)
// *  You are not responsible for freeing newstatus.
// */
//void tox_callback_statusmessage(Tox *tox, void (*function)(Tox *tox, int, uint8_t *, uint16_t, void *),
//                                void *userdata);
procedure tox_callback_statusmessage(tox: TTox; CallBack: TProcStatusMessage; UserData: Pointer);

///* Set the callback for status type changes.
// *  function(int friendnumber, USERSTATUS kind)
// */
//void tox_callback_userstatus(Tox *tox, void (*function)(Tox *tox, int, TOX_USERSTATUS, void *), void *userdata);
procedure tox_callback_userstatus(tox: TTox; CallBack: TProcUserStatus; UserData: Pointer);

///* Set the callback for read receipts.
// *  function(int friendnumber, uint32_t receipt)
// *
// *  If you are keeping a record of returns from m_sendmessage;
// *  receipt might be one of those values, meaning the message
// *  has been received on the other side.
// *  Since core doesn't track ids for you, receipt may not correspond to any message.
// *  In that case, you should discard it.
// */
//void tox_callback_read_receipt(Tox *tox, void (*function)(Tox *tox, int, uint32_t, void *), void *userdata);
procedure tox_callback_read_receipt(tox: TTox; CallBack: TProcReadReceipt; UserData: Pointer);

///* Set the callback for connection status changes.
// *  function(int friendnumber, uint8_t status)
// *
// *  Status:
// *    0 -- friend went offline after being previously online
// *    1 -- friend went online
// *
// *  NOTE: This callback is not called when adding friends, thus the "after
// *  being previously online" part. it's assumed that when adding friends,
// *  their connection status is offline.
// */
//void tox_callback_connectionstatus(Tox *tox, void (*function)(Tox *tox, int, uint8_t, void *), void *userdata);
procedure tox_callback_connectionstatus(tox: TTox; CallBack: TProcConnectionStatus; UserData: Pointer);

///* Use this function to bootstrap the client.
// * Sends a get nodes request to the given node with ip port and public_key.
// */
//void tox_bootstrap(Tox *tox, tox_IP_Port ip_port, uint8_t *public_key);
procedure tox_bootstrap(tox: TTox; ip_port: TToxIpPort; public_key: PByte);

///* Resolves address into an IP address. If successful, sends a "get nodes"
// *   request to the given node with ip, port and public_key to setup connections
// *
// * address can be a hostname or an IP address (IPv4 or IPv6).
// * if ipv6enabled is 0 (zero), the resolving sticks STRICTLY to IPv4 addresses
// * if ipv6enabled is not 0 (zero), the resolving looks for IPv6 addresses first,
// *   then IPv4 addresses.
// *
// *  returns 1 if the address could be converted into an IP address
// *  returns 0 otherwise
// */
//int tox_bootstrap_from_address(Tox *tox, const char *address, uint8_t ipv6enabled,
//                               uint16_t port, uint8_t *public_key);
function tox_bootstrap_from_address(tox: TTox; const address: PAnsiChar; ipv6enabled: Byte; port: Word; public_key: PByte): Integer;


///*  return 0 if we are not connected to the DHT.
// *  return 1 if we are.
// */
//int tox_isconnected(Tox *tox);
function tox_isconnected(tox: TTox): Integer;

///* Run this before closing shop.
// * Free all datastructures. */
//void tox_kill(Tox *tox);
procedure tox_kill(tox: TTox);

///* The main loop that needs to be run at least 20 times per second. */
//void tox_do(Tox *tox);
procedure tox_do(tox: TTox);


///**********GROUP CHAT FUNCTIONS: WARNING WILL BREAK A LOT************/
//
///* Set the callback for group invites.
// *
// *  Function(Tox *tox, int friendnumber, uint8_t *group_public_key, void *userdata)
// */
//void tox_callback_group_invite(Tox *tox, void (*function)(Tox *tox, int, uint8_t *, void *), void *userdata);
procedure tox_callback_group_invite(tox: TTox; CallBack: TProcGroupInvite; UserData: Pointer);


///* SAVING AND LOADING FUNCTIONS: */
//
///*  return size of messenger data (for saving). */
//uint32_t tox_size(Tox *tox);
function tox_size(tox: TTox): Integer;

///* Save the messenger in data (must be allocated memory of size Messenger_size()). */
//void tox_save(Tox *tox, uint8_t *data);
procedure tox_save(tox: TTox; data: PByte);

///* Load the messenger from data of size length. */
//int tox_load(Tox *tox, uint8_t *data, uint32_t length);
function tox_load(tox: TTox; data: PByte; length: Integer): Integer;

// Возвращает результат загрузки библиотеки Tox
function ToxLoaded: Boolean;
var
  IsToxLoaded: Boolean;

implementation


var
	tox_getaddress_: procedure(tox: TTox; address: PByte); cdecl;
	tox_addfriend_: function(tox: TTox; address: PByte; data: PByte; length: Word): Integer; cdecl;
	tox_addfriend_norequest_: function(tox: TTox; client_id: PByte): Integer; cdecl;
	tox_getfriend_id_: function(tox: TTox; client_id: PAnsiChar): Integer; cdecl;
	tox_getclient_id_: function(tox: TTox; friend_id: Integer; client_id: PByte): Integer; cdecl;
	tox_delfriend_: function(tox: TTox; friendnumber: Integer): Integer; cdecl;
	tox_sendmessage_: function(tox: TTox; friendnumber: Integer; message: PByte; length: Integer): Integer; cdecl;
	tox_sendmessage_withid_: function(tox: TTox; friendnumber: Integer; theid: Integer; message: PAnsiChar; length: Integer): Integer; cdecl;
	tox_sendaction_: function(tox: TTox; friendnumber: Integer; action: PAnsiChar; length: Integer): Integer; cdecl;
	tox_setname_: function(tox: TTox; name: PByte; length: Word): Integer; cdecl;
  tox_getselfname_: function(tox: TTox; name: PByte; nlen: Word): Word; cdecl;
  tox_getname_: function(tox: TTox; friendnumber: Integer; name: PByte): Integer; cdecl;
  tox_set_statusmessage_: function(tox: TTox; status: PByte; length: Word): Integer; cdecl;
  tox_set_userstatus_: function(tox: TTox; status: Cardinal): Integer; cdecl;
  tox_get_statusmessage_size_: function(tox: TTox; friendnumber: Integer): Integer; cdecl;
  tox_copy_statusmessage_: function(tox: TTox; friendnumber: Integer; buf: PByte; maxlen: Integer): Integer; cdecl;
  tox_copy_self_statusmessage_: function(tox: TTox; buf: PByte; maxlen: Integer): Integer; cdecl;
  tox_get_userstatus_: function(tox: TTox; friendnumber: Integer): TToxUserStatus; cdecl;
  tox_get_selfuserstatus_: function(tox: TTox): TToxUserStatus; cdecl;
  tox_set_sends_receipts_: procedure(tox: TTox; friendnumber: Integer; yesno: Integer); cdecl;
  tox_get_friendlist_: function(tox: TTox; out out_list: PByte; out_list_length: PInteger): Integer; cdecl;
  tox_callback_friendrequest_: procedure(tox: TTox; CallBack: TProcFriendRequest; UserData: Pointer); cdecl;
  tox_callback_friendmessage_: procedure(tox: TTox; CallBack: TProcFriendMessage; UserData: Pointer); cdecl;
  tox_callback_action_: procedure(tox: TTox; CallBack: TProcAction; UserData: Pointer); cdecl;
  tox_callback_namechange_: procedure(tox: TTox; CallBack: TProcNameChange; UserData: Pointer); cdecl;
  tox_callback_statusmessage_: procedure(tox: TTox; CallBack: TProcStatusMessage; UserData: Pointer); cdecl;
  tox_callback_userstatus_: procedure(tox: TTox; CallBack: TProcUserStatus; UserData: Pointer); cdecl;
  tox_callback_read_receipt_: procedure(tox: TTox; CallBack: TProcReadReceipt; UserData: Pointer); cdecl;
  tox_callback_connectionstatus_: procedure(tox: TTox; CallBack: TProcConnectionStatus; UserData: Pointer); cdecl;
  tox_bootstrap_: procedure(tox: TTox; ip_port: TToxIpPort; public_key: PByte); cdecl;
  tox_isconnected_: function(tox: TTox): Integer; cdecl;
  tox_kill_: procedure(tox: TTox); cdecl;
  tox_do_: procedure(tox: TTox); cdecl;
  tox_size_: function(tox: TTox): Integer; cdecl;
  tox_save_: procedure(tox: TTox; data: PByte); cdecl;
  tox_load_: function(tox: TTox; data: PByte; length: Integer): Integer; cdecl;
  tox_callback_group_invite_: procedure(tox: TTox; CallBack: TProcGroupInvite; UserData: Pointer); cdecl;
  tox_bootstrap_from_address_: function(tox: TTox; const address: PAnsiChar; ipv6enabled: Byte; port: Word; public_key: PByte): Integer; cdecl;


function ToxLoaded: Boolean;
begin
  Result := IsToxLoaded;
end;

procedure tox_getaddress(tox: TTox; address: PByte);
begin
	tox_getaddress_(tox, address);
end;

function tox_addfriend(tox: TTox; address: PByte; data: PByte; length: Word): Integer;
begin
	Result := tox_addfriend_(tox, address, data, length);
end;

function tox_addfriend_norequest(tox: TTox; client_id: PByte): Integer;
begin
	Result := tox_addfriend_norequest_(tox, client_id);
end;

function tox_getfriend_id(tox: TTox; client_id: PAnsiChar): Integer;
begin
	Result := tox_getfriend_id_(tox, client_id);
end;

function tox_getclient_id(tox: TTox; friend_id: Integer; client_id: PByte): Integer;
begin
	Result := tox_getclient_id_(tox, friend_id, client_id);
end;

function tox_delfriend(tox: TTox; friendnumber: Integer): Integer;
begin
	Result := tox_delfriend_(tox, friendnumber);
end;

function tox_sendmessage(tox: TTox; friendnumber: Integer; message: PByte; length: Integer): Integer;
begin
	Result := tox_sendmessage_(tox, friendnumber, message, length);
end;

function tox_sendmessage_withid(tox: TTox; friendnumber: Integer; theid: Integer; message: PAnsiChar; length: Integer): Integer;
begin
	Result := tox_sendmessage_withid_(tox, friendnumber, theid, message, length);
end;

function tox_sendaction(tox: TTox; friendnumber: Integer; action: PAnsiChar; length: Integer): Integer;
begin
	Result := tox_sendaction_(tox, friendnumber, action, length);
end;

function tox_setname(tox: TTox; name: PByte; length: Word): Integer;
begin
  Result := tox_setname_(tox, name, length);
end;

function tox_getselfname(tox: TTox; name: PByte; nlen: Word): Word;
begin
  Result := tox_getselfname_(tox, name, nlen);
end;

function tox_getname(tox: TTox; friendnumber: Integer; name: PByte): Integer;
begin
  Result := tox_getname_(tox, friendnumber, name);
end;

function tox_set_statusmessage(tox: TTox; status: PByte; length: Word): Integer;
begin
  Result := tox_set_statusmessage_(tox, status, length);
end;

function tox_set_userstatus(tox: TTox; status: TToxUserStatus): Integer;
begin
  Result := tox_set_userstatus_(tox, Integer(status));
end;

function tox_get_statusmessage_size(tox: TTox; friendnumber: Integer): Integer;
begin
  Result := tox_get_statusmessage_size_(tox, friendnumber);
end;

function tox_copy_statusmessage(tox: TTox; friendnumber: Integer; buf: PByte; maxlen: Integer): Integer;
begin
  Result := tox_copy_statusmessage_(tox, friendnumber, buf, maxlen);
end;

function tox_copy_self_statusmessage(tox: TTox; buf: PByte; maxlen: Integer): Integer;
begin
  Result := tox_copy_self_statusmessage_(tox, buf, maxlen);
end;

function tox_get_userstatus(tox: TTox; friendnumber: Integer): TToxUserStatus;
begin
  Result := tox_get_userstatus_(tox, friendnumber);
end;

function tox_get_selfuserstatus(tox: TTox): TToxUserStatus;
begin
  Result := tox_get_selfuserstatus_(tox);
end;

procedure tox_set_sends_receipts(tox: TTox; friendnumber: Integer; yesno: Integer);
begin
  tox_set_sends_receipts_(tox, friendnumber, yesno);
end;

function tox_get_friendlist(tox: TTox; out out_list: PByte; out_list_length: PInteger): Integer;
begin
  Result := tox_get_friendlist_(tox, out_list, out_list_length);
end;

procedure tox_callback_friendrequest(tox: TTox; CallBack: TProcFriendRequest; UserData: Pointer);
begin
  tox_callback_friendrequest_(tox, CallBack, UserData);
end;

procedure tox_callback_friendmessage(tox: TTox; CallBack: TProcFriendMessage; UserData: Pointer);
begin
  tox_callback_friendmessage_(tox, CallBack, UserData);
end;

procedure tox_callback_action(tox: TTox; CallBack: TProcAction; UserData: Pointer);
begin
  tox_callback_action_(tox, CallBack, UserData);
end;

procedure tox_callback_namechange(tox: TTox; CallBack: TProcNameChange; UserData: Pointer);
begin
  tox_callback_namechange_(tox, CallBack, UserData);
end;

procedure tox_callback_statusmessage(tox: TTox; CallBack: TProcStatusMessage; UserData: Pointer);
begin
  tox_callback_statusmessage_(tox, CallBack, UserData);
end;

procedure tox_callback_userstatus(tox: TTox; CallBack: TProcUserStatus; UserData: Pointer);
begin
  tox_callback_userstatus_(tox, CallBack, UserData);
end;

procedure tox_callback_read_receipt(tox: TTox; CallBack: TProcReadReceipt; UserData: Pointer);
begin
  tox_callback_read_receipt_(tox, CallBack, UserData);
end;

procedure tox_callback_connectionstatus(tox: TTox; CallBack: TProcConnectionStatus; UserData: Pointer);
begin
  tox_callback_connectionstatus_(tox, CallBack, UserData);
end;

procedure tox_bootstrap(tox: TTox; ip_port: TToxIpPort; public_key: PByte);
begin
  tox_bootstrap_(tox, ip_port, public_key);
end;

function tox_isconnected(tox: TTox): Integer;
begin
  Result := tox_isconnected_(tox);
end;

procedure tox_kill(tox: TTox);
begin
  tox_kill_(tox);
end;

procedure tox_do(tox: TTox);
begin
  tox_do_(tox);
end;

function tox_size(tox: TTox): Integer;
begin
  Result := tox_size_(tox);
end;

procedure tox_save(tox: TTox; data: PByte);
begin
  tox_save_(tox, data);
end;

function tox_load(tox: TTox; data: PByte; length: Integer): Integer;
begin
  Result := tox_load_(tox, data, length);
end;

procedure tox_callback_group_invite(tox: TTox; CallBack: TProcGroupInvite; UserData: Pointer);
begin
  tox_callback_group_invite_(tox, CallBack, UserData);
end;

function tox_bootstrap_from_address(tox: TTox; const address: PAnsiChar; ipv6enabled: Byte; port: Word; public_key: PByte): Integer;
begin
  Result := tox_bootstrap_from_address_(tox, address, ipv6enabled, port, public_key);
end;

{
   Загрузка библиотеки и получение адресов функции
                                                                              }
procedure LoadLibraryTox;
var
	hlib: THandle;
begin
	hlib := LoadLibrary({$IFDEF Win32}'libtoxcore-0.dll'{$ELSE}GetCurrentDir + '/libtoxcore-0.so'{$ENDIF});
  IsToxLoaded := hlib <> 0;

	if not IsToxLoaded then
		Exit;

  tox_getaddress_ := GetProcAddress(hlib, 'tox_getaddress');
	tox_addfriend_ := GetProcAddress(hlib, 'tox_addfriend');
	tox_addfriend_norequest_ := GetProcAddress(hlib, 'tox_addfriend_norequest');
	tox_getfriend_id_ := GetProcAddress(hlib, 'tox_getfriend_id');
	tox_getclient_id_ := GetProcAddress(hlib, 'tox_getclient_id');
	tox_delfriend_ := GetProcAddress(hlib, 'tox_delfriend');
	tox_sendmessage_ := GetProcAddress(hlib, 'tox_sendmessage');
	tox_sendmessage_withid_ := GetProcAddress(hlib, 'tox_sendmessage_withid');
  tox_sendaction_ := GetProcAddress(hlib, 'tox_sendaction');
  tox_setname_ := GetProcAddress(hlib, 'tox_setname');
  tox_getselfname_ := GetProcAddress(hlib, 'tox_getselfname');
  tox_getname_ := GetProcAddress(hlib, 'tox_getname');
  tox_set_statusmessage_ := GetProcAddress(hlib, 'tox_set_statusmessage');
  tox_set_userstatus_ := GetProcAddress(hlib, 'tox_set_userstatus');
  tox_get_statusmessage_size_ := GetProcAddress(hlib, 'tox_get_statusmessage_size');
  tox_copy_statusmessage_ := GetProcAddress(hlib, 'tox_copy_statusmessage');
  tox_copy_self_statusmessage_ := GetProcAddress(hlib, 'tox_copy_self_statusmessage');
  tox_get_userstatus_ := GetProcAddress(hlib, 'tox_get_userstatus');
  tox_get_selfuserstatus_ := GetProcAddress(hlib, 'tox_get_selfuserstatus');
  tox_set_sends_receipts_ := GetProcAddress(hlib, 'tox_set_sends_receipts');
  tox_get_friendlist_ := GetProcAddress(hlib, 'tox_get_friendlist');
  tox_callback_friendrequest_ := GetProcAddress(hlib, 'tox_callback_friendrequest');
  tox_callback_friendmessage_ := GetProcAddress(hlib, 'tox_callback_friendmessage');
  tox_callback_action_ := GetProcAddress(hlib, 'tox_callback_action');
  tox_callback_namechange_ := GetProcAddress(hlib, 'tox_callback_namechange');
  tox_callback_statusmessage_ := GetProcAddress(hlib, 'tox_callback_statusmessage');
  tox_callback_userstatus_ := GetProcAddress(hlib, 'tox_callback_userstatus');
  tox_callback_read_receipt_ := GetProcAddress(hlib, 'tox_callback_read_receipt');
  tox_callback_connectionstatus_ := GetProcAddress(hlib, 'tox_callback_connectionstatus');
  tox_bootstrap_ := GetProcAddress(hlib, 'tox_bootstrap');
  tox_isconnected_ := GetProcAddress(hlib, 'tox_isconnected');
  tox_kill_ := GetProcAddress(hlib, 'tox_kill');
  tox_do_ := GetProcAddress(hlib, 'tox_do');
  tox_size_ := GetProcAddress(hlib, 'tox_size');
  tox_save_ := GetProcAddress(hlib, 'tox_save');
  tox_load_ := GetProcAddress(hlib, 'tox_load');
  tox_callback_group_invite_ := GetProcAddress(hlib, 'tox_callback_group_invite');
	tox_bootstrap_from_address_ := GetProcAddress(hlib, 'tox_bootstrap_from_address');
end;

initialization
	LoadLibraryTox;

end.
