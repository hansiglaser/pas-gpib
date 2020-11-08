
FPC = fpc
FPOPTS = -Fu../base -Fu../devcom -Fu../usb -Fu../pas-libusb/src -Fu../gpib -Fu../instruments -Fu../instruments/keithley -Fu../instruments/lecroy -Fu../instruments/rohdeschwarz -Fu../instruments/agilent

PROGS = fpgacurrentmeasurement testkeithley2010 testlecroywavejet3xx testrohdeschwarzfse testusbtmc testagilent34410a testagilentmsox3000a serialparse

all: $(PROGS)

fpgacurrentmeasurement: fpgacurrentmeasurement.pas
	-$(FPC) $(FPOPTS) $<

testkeithley2010: testkeithley2010.pas
	-$(FPC) $(FPOPTS) $<

testlecroywavejet3xx: testlecroywavejet3xx.pas
	$(FPC) $(FPOPTS) $<

testrohdeschwarzfse: testrohdeschwarzfse.pas
	-$(FPC) $(FPOPTS) $<

testusbtmc: testusbtmc.pas
	$(FPC) $(FPOPTS) $<

testagilent34410a: testagilent34410a.pas
	$(FPC) $(FPOPTS) $<

testagilentmsox3000a: testagilentmsox3000a.pas
	$(FPC) $(FPOPTS) $<

serialparse: serialparse.lpr serialparsei2c.pas serial-i2c-lex.pas serial-i2c-parser.pas
	$(FPC) $(FPOPTS) $<

serial-i2c-parser.pas: serial-i2c-parser.y
	pyacc serial-i2c-parser.y

serial-i2c-lex.pas: serial-i2c-lex.l serial-i2c-parser.pas
	plex serial-i2c-lex.l

clean:
	rm -f $(PROGS) *.o *.ppu *~ *.compiled *.or wj-*.png *-parser.pas *-lex.pas
