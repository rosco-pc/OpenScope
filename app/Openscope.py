try:
  from PySide import QtCore, QtGui
except ImportError:
  from PyQt4 import QtCore, QtGui
  
import Openscope_ui
from scopeview import ScopeView
import sys

class ControlMainWindow(QtGui.QMainWindow):
  buttons = {"scope_function": [0, 4, {0: "Scope", 1: "FFT", 2: "LSA", 3:"Combined"}],
             "scope_start"   : [0, 2, {0: "Start", 1: "Stop"}],
             "ch1_probe"     : [0, 2, {0: "x1", 1: "x10"}],
             "ch1_offset"    : [0, 3, {0: "AC", 1: "DC",2: "None"}],
             "ch2_probe"     : [0, 2, {0: "x1", 1: "x10"}],
             "ch2_offset"    : [0, 3, {0: "AC", 1: "DC",2: "None"}],
             "ch3_set"       : [0, 2, None],
             "ch4_set"       : [0, 2, None],
             "ch5_set"       : [0, 2, None],
             "ch6_set"       : [0, 2, None],
             "ch7_set"       : [0, 2, None],
             "freq_shape"    : [0, 4, {0: "Sinus", 1: "Square", 2:"Sawtooth", 3:"Triangle"}],
             "freq_range"    : [0, 3, {0:"Hz",1:"kHz",2:"MHz"}]
            } 
              
  
  def __init__(self, parent=None):
    super(ControlMainWindow, self).__init__(parent)
    self.ui =  Openscope_ui.Ui_MainWindow()
    self.ui.setupUi(self)
    self.scope = Scope()
    self.prev = 0
    self.freq = 0
    
    # Connect widgets to make things happen
    # Scope buttons
    self.ui.scope_start.clicked.connect(self.buttonClicked)
    self.ui.scope_function.clicked.connect(self.buttonClicked)
    # Time base
    self.ui.time_base.valueChanged.connect(self.dialChanged)
    # Channel 1
    self.ui.ch1_resolution.valueChanged.connect(self.dialChanged)
    self.ui.ch1_probe.clicked.connect(self.buttonClicked)
    self.ui.ch1_offset.clicked.connect(self.buttonClicked)
    # Channel 2
    self.ui.ch2_resolution.valueChanged.connect(self.dialChanged)
    self.ui.ch2_probe.clicked.connect(self.buttonClicked)
    self.ui.ch2_offset.clicked.connect(self.buttonClicked)
    
    # expansion card
    # Channel selection
    self.ui.ch3_set.clicked.connect(self.buttonClicked)
    self.ui.ch4_set.clicked.connect(self.buttonClicked)
    self.ui.ch5_set.clicked.connect(self.buttonClicked)
    self.ui.ch6_set.clicked.connect(self.buttonClicked)
    self.ui.ch7_set.clicked.connect(self.buttonClicked)
    # Trigger setting
    self.ui.lsa_trigger.clicked.connect(self.triggerDialog)
    # Frequency generator
    #self.ui.freq_display.textChanged.connect(self.newFreq)
    self.ui.freq_dial.valueChanged.connect(self.changeFreq)
    self.ui.freq_shape.clicked.connect(self.buttonClicked)
    self.ui.freq_range.clicked.connect(self.buttonClicked)

  def buttonClicked(self):
    "Button has been clicked, perform requested action"
    # Get button name
    el = self.sender()
    name = str(el.objectName())
    # Get button details
    button = self.buttons[name]
    # Update button state
    button[0] += 1
    button[0] %= button[1]
    # Change button text
    if button[2]:
      el.setText(button[2][button[0]])
    # Send to PropScope
    data = name.split("_")
    setting(data[1].upper(), data[0], button[0])

  def dialChanged(self, value):
    "Dial has changed value, update ScopeView and device"
    # Get dial name
    el = self.sender()
    dial = str(el.objectName())
    # Send new value to PropScope
    data = dial.split("_")
    setting(data[1].upper(), data[0], value)

  def triggerDialog(self):
    return
  
  def newFreq(self, text):
    self.freq = text
    
  def changeFreq(self, value):
    "Change signal frequency"
    step = value - self.prev
    # Handle 'roll-over' of dial 
    if step > 50:             # moving down
      step = -(self.prev + (99 - value))
    elif step < -50 :         # moving up
      step = value + (99 - self.prev)
    self.freq += step
    # Check limits
    if self.freq < 0:
      self.freq = 0
    elif self.freq > 10000000:
      self.freq = 9999999
    # Save value for next step calculation
    self.prev = value
    # Update display
    self.ui.freq_display.display(self.freq)
    # Send to PropScope
    setting("VALUE", "freq", self.freq)

def setting(func, el, value):
  print "Set %s = %s for %s"%(func, value, el)
##  e = {"time":0,
##       "scope":0,
##       "lsa":0,
##       "ch1":0,
##       "ch2":1,
##       "ch3":2,
##       "ch4":3,
##       "ch5":4,
##       "ch6":5,
##       "ch7":6,
##       "freq":7}
##  f = {"RESOLUTION":  (False, self.setBase),
##       "RANGE":       (False, self.setRange)
##       "PROBE":       (False, self.setProbe),
##       "FUNCTION":    (False, self.setFunction),
##       "SHOW":        (False, self.),
##       "SELECT":      (True, 0),
##       "START":       (True, 16),
##       "TRIGGER":     (True, 17),
##       "FREQUENCY":   (True, 18),
##       "OFFSET":      (True, 19)}
##       
##  if f[func][0]:
##    res = send(SET,e[el]+f[func], value)
##  else:
##    
  return

class Scope:
  INIT  = 0x00
  START = 0x01
  SET   = 0x02
  GET   = 0x03
  
  def __init__(self):
    self.prop = Propeller()
    res = prop.send(cmd=INIT)
    if res[:15] != "OpenScope v1.0 ":
      if not self.prop.loadFirmware("firmware/openscope10.bin"):
        print "PropScope not detected"
      else:
        res = self.send(INIT)
    return
  def send(cmd, **args):
    return
  
class Propeller:
  def __init__(self):
    return

if __name__ == "__main__":
    app = QtGui.QApplication(sys.argv)
    mySW = ControlMainWindow()
    mySW.show()
    sys.exit(app.exec_())
