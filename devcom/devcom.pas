Unit DevCom;

{$mode objfpc}{$H+}

// use interfaces without IUnknown as ancestor, so they don' have reference
// counting, the classes don't need to be derived from TInterfacedObject, and
// there are no strange problems when destroying objects using our classes
{$interfaces corba}

Interface

Type
  { IDeviceCommunicator }

  (**
   * Device Communicator interface definition
   *
   * Basic device communication interface definition to send messages to
   * and receive messages from devices.
   *
   *)
  IDeviceCommunicator = interface
    /// Send a message to the device
    Procedure Send(St:String);
    /// Wait until a message is received
    Function  Receive : String;
    /// Send a message and wait until a response messages is received
    Function  Query(St:String):String;
    (**
     * Set timeout for Receive and Query in us
     *
     * Note: The actual value can differ from the specified timeout due to
     * a different resolution provided by the communication interface.
     *)
    Procedure SetTimeout(ATimeout:LongInt);
  End;

Implementation

End.

