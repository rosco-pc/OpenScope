try:
  from PySide import QtCore, QtGui
except ImportError:
  from PyQt4 import QtCore, QtGui
  

class ScopeView(QtGui.QGraphicsView):
    def __init__(self, parent):
      super(ScopeView, self).__init__(parent)
      self.scene = QtGui.QGraphicsScene(self)
      self.setScene(self.scene)
      img = QtGui.QImage(512, 420, QtGui.QImage.Format_RGB16)
      img.fill(QtCore.Qt.white)
      self.drawGraticule(img)
      self.setBackgroundBrush(QtGui.QBrush(img))
    
    def drawGraticule(self, img):
      "Renders an empty graticule"
      # The graticule is divided into 10 columns x 10 rows
      # Each cell is 50x40 pixels large, with 5 subdivisions per
      # cell, meaning 10x8 pixels each. Subdivision lines are
      # displayed on the central X and Y axis
      # Active area = 6,10 to 506,410 (500x400 pixels)
      
      start_x = 6
      end_x   = 506
      start_y = 10
      end_y   = 410
      h       = 400
      w       = 500
      
      pen = QtGui.QPen(QtCore.Qt.gray, 1, QtCore.Qt.SolidLine)
      qp = QtGui.QPainter(img)
      qp.setPen(pen)
      step = 50
      # Vertical lines (50 pixels apart)
      step = 50
      for i in range(0, 9):
        x = start_x + step +i*step
        qp.drawLine(x, 10, x, 410)
      # Horizontal lines (40 pixels apart)
      step = 40
      for i in range(0, 9):
        y = start_y + step + i*step
        qp.drawLine(6, y, 506, y)
        
      pen.setColor(QtCore.Qt.black)
      # Vertical sub-divisions (8 pixels apart)
      step = 8
      x1 = start_x + w/2 - 2
      x2 = start_x + w/2 + 2
      for i in range(1, 50):
        y = start_y + i*step
        qp.drawLine(x1, y, x2, y)
      # Horizontal sub-divisions (10 pixels apart)
      step = 10
      y1 = start_y + h/2 - 2
      y2 = start_y + h/2 + 2
      for i in range(1, 50):
        x = start_x + i*step
        qp.drawLine(x, y1, x, y2)

