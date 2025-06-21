=====================================================================
pas-gpib -- Full measurement framework for GPIB, USB-TMC, TCP/IP, ...
=====================================================================

**pas-gpib** is a full framework to perform measurements with professional
instruments controlled via GPIB, USB-TMC, TCP/IP, RS-232, ...

The framework is written in Pascal using `FreePascal
<http://www.freepascal.org/>`_. It has a Pascal header translations for
Linux GPIB including an object-oriented wrapper. A USB Test and Measurement
class implementation is included.

Although this is work in progress, several protocols and instrumets are fully
supported.


Design Principle
================

This package is organized into two major parts

 - instruments
 - device communicators

For each **instrument** (e.g., multimeter, scope, spectrum analyzer, ...) a
dedicated class is implemented, which derives from ``TRemoteInstrument``. These
use a **device communicator** instance, which is a class which implements the
``IDeviceCommunicator`` interface.

See ``doc/UML.dia`` for an UML class diagram.

The ``TRemoteInstrument`` class holds an instance of an ``IDeviceCommunicator``
class. Therefore, to create an instrument object (e.g., ``TLeCroyWaveJet``,
which is derived from ``TRemoteInstrument``), you have to supply an instance
of a class implementing ``IDeviceCommunicator``, e.g.,
``TTCPLeCroyCommunicator``.


Device Communicators
====================

Currently, there are five device communicator classes

 - ``TUSBTMCCommunicator``
 - ``TUSBLeCroyCommunicator``
 - ``TRS232Communicator``
 - ``TTCPCommunicator``
 - ``TTCPLeCroyCommunicator``
 - ``TGPIBCommunicator``

These are implemented in Pascal units in ``devcom/``.

The ``TUSBTMCCommunicator`` provides communication to instruments with a
USB Test and Measurement Class (USB-TMC) interface [USBTMC10]_. It uses
``TUSBTMCUSB488``, which implements the USB-TMC USB488 Subclass [USB488]_ to
provide IEEE-488 (GPIB) communication via USB. ``TUSBTMCUSB488`` is derived
from ``TUSBTMCBase``, which implements the base USB-TMC communication. These
classes are implemented in units in ``usb/``.

.. [USBTMC10] Universal Serial Bus Test and Measurement Class Specification
   (USBTMC), Revision 1.0, April 14, 2003

.. [USB488] Universal Serial Bus Test and Measurement Class, Subclass USB488
   Specification (USBTMC-USB488), Revision 1.0, April 14, 2003

.. [Truevolt] Keysight Truevolt Series Digital Multimeters Operating and
   Service Guide, Part Number: 34460-90901, Edition 5, (August 18, 2017)

.. [E36300Prg] Keysight E36300 Series Programmable DC Power Supplies
   Programming Guide, Manual Part Number E36311-90008, Edition 5, May 2018

.. [E36200Prg] Keysight E36200 Series Programming Guide, Manual Part Number
   E36200-90008, Edition 2, June 2023

.. [EL3000Prg] Keysight EL30000 Series Programming Guide, Manual Part Number
   EL34243-90007, Edition 3, October 2023

The ``TUSBLeCroyCommunicator`` provides communication with LeCroy scopes which
do not adhere to the USB-TMC specification. These include the WaveAce and
WaveJet devices. It uses a `` TUSBLeCroy``, which implements a driver for the
proprietary protocol. This was reverse engineered with the Windows "virtual COM
port" driver.

The ``TGPIBCommunicator`` provides communication to instruments with an
IEEE-488 (GPIB) interface. It uses ``TGPIB``, an object oriented wrapper for
`Linux GPIB <http://linux-gpib.sourceforge.net/>`_. These are implemented in
units in ``gpib/``.

The class ``TTCPCommunicator`` provides basic TCP/IP communication to a host
at a given TCP port. The descendant class ``TTCPLeCroyCommunicator`` adds a
special header used by several LeCroy scopes when communicating via TCP/IP.
Their protocol was reverse-engineered.


Instruments
===========

Currently, rudimentary support for three devices is available

 - ``TLeCroyWaveJet`` for LeCroy WaveJet scopes
 - ``TKeithley2010`` for Keithley 2010 multimeters
 - ``TRohdeSchwarzFSEB`` for Rohde&Schwarz FSEB spectrum analyzers
 - ``TAgilent34410A`` for Agilent 34410A and Keysight 34461A digital multimeters
 - ``TKeysightE3631xA`` for Keysight E36300A and E36200A programmable DC power supplies
 - ``TKeysightEL30000`` for Keysight EL30000 Series DC Electronic Loads
 - ``TKeithley2600`` for Keithley 2602A SourceMeter SMU Instruments
 - ``TKeithley2450`` for Keithley 2450 SourceMeter SMU Instruments
 - ``TKeithleyDMM6500`` for Keithley DMM6500 Digital Multimeters


Verified
========

 - ``testusbtmc`` correctly lists (at least one) USB-TMC capable device and
   performs "``*IDN?``".
 - ``testagilent34410a`` correctly communicates via USB-TMC as well as TCP with
   the device and performs a test measurement.
 - ``testkeysighte3631xa`` correctly communicates via USB-TMC as well as TCP with
   the device and performs tests.
 - ``testlecroywavejet3xx`` correctly communicates via TCP as well as USB with
   the device, performs settings and saves a screenshot. 
 - ``testkeithley2600`` correctly communicates via TCP with the device and
   performs settings and measurements.


Directory Structure
===================

Directory structure::

  doc/
  devcom/
  instruments/
    agilent/
    keithley/
    lecroy/
    rohdeschwarz/
  gpib/
  usb/
  pas-libusb/


References
==========

The USB-TMC implementation uses libusb(x) 1.0 (see http://www.libusb.org/
and http://libusbx.sourceforge.net/ and the Pascal OOP wrapper at
https://github.com/hansiglaser/pas-libusb/tree/libusb-1.0

This is referenced using `Git Submodules
<http://git-scm.com/book/en/Git-Tools-Submodules>`_. After cloning this
project, you have to add the submodules too.

::

  git clone https://github.com/hansiglaser/pas-gpib.git
  cd pas-gpib
  git submodule init
  git submodule update

The submodule was added using the command (Important: https:// URL, not
git:// URL, because github doesn't support "push" to git:// URLs!)

::

  git submodule add -b libusb-1.0 https://github.com/hansiglaser/pas-libusb.git pas-libusb  
  git commit

To change a submodule within this main project and then commit and push to
GitHub, a few things must be `considered
<http://longweekendmobile.com/2010/11/05/making-changes-in-a-git-submodule-made-simple/>`_.
A submodule by default is a 'Detached Head' this means it isn't on a branch.

::

  cd pas-libusb/
  git checkout libusb-1.0    # switch to a branch
  # make changes
  git add ...                # stage changes
  git commit                 # commit
  git push                   # and push to GitHub
  cd ../..
  git submodule              # shows that submodules are at a newer state
                             # than referenced by the main project
  git add pas-libusb         # tell git to use the most current revision of
                             # this submodule
  git commit                 # commit
  git push                   # and push to GitHub


TODO
====

 - FSEB: search manual, add functions and stuff to build test program
 - document how to get Linux-GPIB in Debian (with direct SVN checkout or so)
 - add lots of devices
    - Agilent E3631A Power Supply
    - Agilent 33220A Arbitrary Waveform Generator
 - add many functions to devices, always specify reference manual
 - add test programs
    - Rohde&Schwarz FSEB to get the nice image with satellite spectrum
    - Keithley 2010 with the switcher card to demonstrate the settling time
      (and add a comment on this example to TKeithley2010.SelectChannel)
 - dedicated section in this README about the test programs
 - TKeithely2010:
    - generalize to 2000
    - select DCV/ACV/DCI/ACI/Ohm2/Ohm4/Freq/Temp, many more functions
 - TKeithley2600:
    - add methods PrintBuffer and PrintNumber, care for "Trim" in Query()!
    - add method SetDataFormat which sets  "format.data = ..." and stores this
      information for later use by PrintBuffer and PrintNumber; but hide
      "format.byteorder" and always set it to what is easiest for the local
      machine; or include this with SetDataFormat
    - flexible buffer handling (MakeBuffer, ...)
 - add communicator for LXI (LAN eXtensions for Instrumentation)
   http://en.wikipedia.org/wiki/LAN_eXtensions_for_Instrumentation
   mDNS/DNS-SD, SCPI Port (Telnet?)
 - finish implementation of USB488, carefully read the spec
 - License: use modified LGPL, the individual instrument drivers only have a
   few basic functions, actively encourage users of pas-gpib to contribute their
   added methods (which is required by the license, except they derive from the
   class) and their added instruments
 - document: Standard Commands for Programmable Instruments (SCPI)
 - see also TCL libraries http://wiki.tcl.tk/14780 and
   http://gpib-tcl.sourceforge.net/GPIB-Tcl.html

