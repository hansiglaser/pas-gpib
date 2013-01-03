
FPC = fpc
FPOPTS = -Fu../base -Fu../devcom -Fu../usb -Fu../pas-libusb/src -Fu../gpib -Fu../instruments -Fu../instruments/keithley -Fu../instruments/lecroy -Fu../instruments/rohdeschwarz -Fu../instruments/agilent

PROGS = fpgacurrentmeasurement testkeithley2010 testlecroywavejet3xx testrohdeschwarzfse testusbtmc testagilent34410a

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

clean:
	rm -f $(PROGS) *.o *.ppu *~ *.compiled *.or wj-*.png
