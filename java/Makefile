
CLASSPATH = /usr/java/classes:.

WEBTEST = /ftp/babbage/test
WEBREL = /ftp/babbage
WEBSIMDIR = $(WEBREL)/websim
ARCHDIR = $(WEBREL)/release
EXAMPLEDIR = $(WEBREL)/Examples

#OPTIMISE = -O
OPTIMISE = -g

#   Object file component definitions

SIMULATOR_CORE = \
		 analyticalEngine.class 		    \
		 AnnunciatorPanel.class 		    \
		 Attendant.class			    \
		 BigInt.class				    \
		 Card.class				    \
		 CardPunchingApparatus.class		    \
		 CardReader.class			    \
		 CardSource.class			    \
		 CurveDrawingApparatus.class		    \
		 Mill.class				    \
		 OutputApparatus.class			    \
		 PrintingApparatus.class		    \
		 Store.class

COMMAND_LINE =	\
		aes.class				    \
		curvePlot.class 			    \
		frameCurveDrawingApparatus.class

JAVA_APPLET =	\
		aeapp.class				    \
		AnimatedAnnunciatorPanel.class		    \
		AnimatedPrintingApparatus.class 	    \
		annunciatorDisplay.class		    \
		attendantDisplay.class			    \
		cardReaderDisplay.class 		    \
		graphicalLabel.class			    \
		loadPanel.class 			    \
		millDisplay.class			    \
		printerDisplay.class			    \
		storeDisplay.class			    \
		WebCurvePlot.class			    \
		WebFrameCurveDrawingApparatus.class	    \
		Util.class

C_ALL = $(SIMULATOR_CORE) $(COMMAND_LINE) $(JAVA_APPLET)

#   Source code component definitions

S_SIMULATOR_CORE = \
		analyticalEngine.java			    \
		AnnunciatorPanel.java			    \
		Attendant.java				    \
		BigInt.java				    \
		CardPunchingApparatus.java		    \
		CardReader.java 			    \
		CurveDrawingApparatus.java		    \
		Mill.java				    \
		OutputApparatus.java			    \
		PrintingApparatus.java			    \
		Store.java

S_COMMAND_LINE = \
		aes.java				    \
		frameCurveDrawingApparatus.java

S_JAVA_APPLET = \
		aeapp.java				    \
		WebFrameCurveDrawingApparatus.java

S_ALL = Makefile $(S_SIMULATOR_CORE) $(S_COMMAND_LINE) $(S_JAVA_APPLET)


all:	commandline applet

applet: $(SIMULATOR_CORE) $(JAVA_APPLET)

commandline:	$(SIMULATOR_CORE) $(COMMAND_LINE)


test:	all
	appletviewer ae.html

clean:
	rm -f *.bak *.class aesource.tar.gz aeclass.tar.gz

websim: applet
	rm -f $(WEBSIMDIR)/*.class
	cp -p $(SIMULATOR_CORE) $(JAVA_APPLET) $(WEBSIMDIR)

archive: all
	rm -f $(ARCHDIR)/aesource.zip $(ARCHDIR)/aeclass.zip
	zip $(ARCHDIR)/aesource.zip $(S_ALL)
	zip $(ARCHDIR)/aeclass.zip $(C_ALL)

examples:
	rm -f $(ARCHDIR)/examples.zip
	( cd $(EXAMPLEDIR) ; zip $(ARCHDIR)/examples.zip *.ae )

webtest: all
#	rm -rf $(WEBTEST)
#	tar cfv /tmp/a.tar .
#	mkdir $(WEBTEST)
#	( cd $(WEBTEST) ; tar xfv /tmp/a.tar )

webrel: all
#	rm -rf $(WEBREL)
#	tar cfv /tmp/a.tar .
#	mkdir $(WEBREL)
#	( cd $(WEBREL) ; tar xfv /tmp/a.tar )



.java.class:
	javac $(OPTIMISE) -classpath $(CLASSPATH) $*.java

.SUFFIXES:  .java .class
