#tag Window
Begin Window MainWindow
   BackColor       =   &cFFFFFF00
   Backdrop        =   0
   CloseButton     =   True
   Compatibility   =   ""
   Composite       =   False
   Frame           =   0
   FullScreen      =   False
   FullScreenButton=   False
   HasBackColor    =   False
   Height          =   481
   ImplicitInstance=   True
   LiveResize      =   True
   MacProcID       =   0
   MaxHeight       =   481
   MaximizeButton  =   True
   MaxWidth        =   421
   MenuBar         =   1632753663
   MenuBarVisible  =   True
   MinHeight       =   481
   MinimizeButton  =   True
   MinWidth        =   421
   Placement       =   1
   Resizeable      =   True
   Title           =   "Boss 98"
   Visible         =   True
   Width           =   421
   Begin PushButton StartButton
      AutoDeactivate  =   True
      Bold            =   False
      ButtonStyle     =   "0"
      Cancel          =   False
      Caption         =   "Start"
      Default         =   True
      Enabled         =   True
      Height          =   20
      HelpTag         =   ""
      Index           =   -2147483648
      InitialParent   =   ""
      Italic          =   False
      Left            =   321
      LockBottom      =   False
      LockedInPosition=   False
      LockLeft        =   True
      LockRight       =   False
      LockTop         =   True
      Scope           =   0
      TabIndex        =   0
      TabPanelIndex   =   0
      TabStop         =   True
      TextFont        =   "System"
      TextSize        =   0.0
      TextUnit        =   0
      Top             =   449
      Transparent     =   False
      Underline       =   False
      Visible         =   True
      Width           =   80
   End
   Begin PushButton ClearButton
      AutoDeactivate  =   True
      Bold            =   False
      ButtonStyle     =   "0"
      Cancel          =   False
      Caption         =   "Clear"
      Default         =   False
      Enabled         =   True
      Height          =   20
      HelpTag         =   ""
      Index           =   -2147483648
      InitialParent   =   ""
      Italic          =   False
      Left            =   229
      LockBottom      =   False
      LockedInPosition=   False
      LockLeft        =   True
      LockRight       =   False
      LockTop         =   True
      Scope           =   0
      TabIndex        =   1
      TabPanelIndex   =   0
      TabStop         =   True
      TextFont        =   "System"
      TextSize        =   0.0
      TextUnit        =   0
      Top             =   449
      Transparent     =   False
      Underline       =   False
      Visible         =   True
      Width           =   80
   End
   Begin Label scoreLabel
      AutoDeactivate  =   True
      Bold            =   False
      DataField       =   ""
      DataSource      =   ""
      Enabled         =   True
      Height          =   20
      HelpTag         =   ""
      Index           =   -2147483648
      InitialParent   =   ""
      Italic          =   False
      Left            =   20
      LockBottom      =   False
      LockedInPosition=   False
      LockLeft        =   True
      LockRight       =   False
      LockTop         =   True
      Multiline       =   False
      Scope           =   0
      Selectable      =   False
      TabIndex        =   3
      TabPanelIndex   =   0
      TabStop         =   True
      Text            =   "Score: 0  Low score: ?"
      TextAlign       =   1
      TextColor       =   &c00000000
      TextFont        =   "System"
      TextSize        =   0.0
      TextUnit        =   0
      Top             =   449
      Transparent     =   False
      Underline       =   False
      Visible         =   True
      Width           =   197
   End
End
#tag EndWindow

#tag WindowCode
	#tag Event
		Function MouseDown(X As Integer, Y As Integer) As Boolean
		  dim i, j, tempx, tempy as integer
		  
		  if StartButton.Enabled = false then
		    mdx = 0
		    mdy = 0
		    tempx = (x-1) \ 42 + 1
		    tempy = (y-1) \ 42 + 1
		    
		    if (x-1) mod 42 > 0 and (y-1) mod 42 > 0 and tempx > 0 and tempx < 11 and tempy > 0 and tempy < 11 then
		      mdx = tempx
		      mdy = tempy
		      if grid(mdx-1,mdy-1) = "" then
		        for i = 1 to 10
		          for j = 1 to 10
		            if i=mdx or j=mdy then
		              gridhl(i-1,j-1) = true
		            else
		              gridhl(i-1,j-1) = false
		            end
		          next
		        next
		        Refresh
		      end
		    end
		    return true
		  else
		    return false
		  end
		  
		End Function
	#tag EndEvent

	#tag Event
		Sub MouseUp(X As Integer, Y As Integer)
		  dim bsc,drag as integer
		  
		  updateHL
		  
		  if mdx > 0 and mdy > 0 then
		    mux = (x-1) \ 42 + 1
		    muy = (y-1) \ 42 + 1
		    drag = abs(mdx - mux)+abs(mdy - muy)
		    
		    if drag = 0 then
		      if mdx > 1 then
		        if grid(mdx-2,mdy-1) = "" then
		          mux = mdx - 1
		          bsc = bsc + 1
		        end
		      end
		      if mdx < 10 then
		        if grid(mdx,mdy-1) = "" then
		          mux = mdx + 1
		          bsc = bsc + 1
		        end
		      end
		      if mdy > 1 then
		        if grid(mdx-1,mdy-2) = "" then
		          muy = mdy - 1
		          bsc = bsc + 1
		        end
		      end
		      if mdy < 10 then
		        if grid(mdx-1,mdy) = "" then
		          muy = mdy + 1
		          bsc = bsc + 1
		        end
		      end
		      if bsc = 1 then
		        drag = 1
		      end
		    end
		    
		    if grid(mdx-1,mdy-1) <> "" and grid(mux-1,muy-1) = "" and drag = 1 then
		      if (x-1) mod 42 > 0 and (y-1) mod 42 > 0 and mux > 0 and mux < 11 and muy > 0 and muy < 11 then
		        grid(mux-1,muy-1) = grid(mdx-1,mdy-1)
		        grid(mdx-1,mdy-1) = ""
		        updateHL
		        updateLabels
		        Refresh
		      end
		    end
		  end
		  
		  if gameOver then
		    clearAction
		  end
		  
		End Sub
	#tag EndEvent

	#tag Event
		Sub Paint(g As Graphics, areas() As REALbasic.Rect)
		  dim i,j as integer
		  
		  for i=1 to 10
		    for j=1 to 10
		      displayletter g,i,j,grid(i-1,j-1)
		    next
		  next
		  
		End Sub
	#tag EndEvent


	#tag Method, Flags = &h0
		Sub clearAction()
		  dim i,j as Integer
		  
		  if score > highscore then
		    highscore = score
		    self.Title = "Word Crush 10x10 - High Score: " + str(highscore)
		  end
		  unplaced = ""
		  for i=0 to 9
		    for j=0 to 9
		      grid(i,j) = ""
		    next
		  next
		  ClearButton.Caption = "Clear HS"
		  StartButton.Enabled = true
		  longword = ""
		  Refresh
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub displayletter(g as graphics, x as integer, y as integer, letter as string)
		  if gridhl(x-1,y-1) then
		    g.foreColor = rgb(191,191,95)
		  else
		    g.foreColor = rgb(255,255,191)
		  end
		  g.fillrect x*42-40,y*42-40,39,39
		  g.foreColor = rgb(0,0,0)
		  g.TextFont="Courier"
		  g.TextSize=36
		  g.DrawString letter, x*42-32, y*42-9
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function isWord(word as String) As Boolean
		  dim sql as string
		  sql = "SELECT * from Words WHERE Word='"+word+"'"
		  
		  dim data as RecordSet
		  data = app.wordsDB.SQLSelect(sql)
		  
		  if data.EOF then
		    return false
		  else
		    return true
		  end
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub updateHL()
		  dim hl,i,j,k,l as integer
		  dim sa, sd as string
		  
		  for i = 1 to 10
		    for j = 1 to 10
		      gridhl(i-1,j-1) = false
		    next
		  next
		  
		  for i = 1 to 10
		    for j = 1 to 9
		      sa = ""
		      sd = ""
		      for k = j to 10
		        sa = sa + if(grid(i-1,k-1)=""," ",grid(i-1,k-1))
		        if len(sa) > 2 then
		          if isWord(sa) then
		            for l = 1 to len(sa)
		              gridhl(i-1,l+j-2) = true
		            next
		          end
		        end
		        sd = sd + if(grid(k-1,i-1)=""," ",grid(k-1,i-1))
		        if len(sd) > 2 then
		          if isword(sd) then
		            for l = 1 to len(sd)
		              gridhl(l+j-2,i-1) = true
		            next
		          end
		        end
		      next
		    next
		  next
		  
		  for i = 1 to 10
		    for j = 1 to 10
		      if gridhl(i-1,j-1) then
		        hl = hl + 1
		      end
		    next
		  next
		  
		  if hl = 98 then
		    gameOver = true
		  end
		  
		  Refresh
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub updateLabels()
		  scoreLabel.Text = "Score: " + str(score) + "  Low score: " + str(0)
		  
		End Sub
	#tag EndMethod


	#tag Property, Flags = &h0
		gameOver As Boolean
	#tag EndProperty

	#tag Property, Flags = &h0
		grid(9,9) As String
	#tag EndProperty

	#tag Property, Flags = &h0
		gridhl(9,9) As Boolean
	#tag EndProperty

	#tag Property, Flags = &h0
		highscore As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		letters As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		longword As String
	#tag EndProperty

	#tag Property, Flags = &h0
		mdx As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		mdy As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		mux As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		muy As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		score As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		unplaced As String
	#tag EndProperty


	#tag Constant, Name = tiles, Type = String, Dynamic = False, Default = \"AAAAAAAAABBCCDDDDEEEEEEEEEEEEFFGGGHHIIIIIIIIIJKLLLLMMNNNNNNOOOOOOOOPPQRRRRRRSSSSTTTTTTUUUUVVWWXYYZ", Scope = Public
	#tag EndConstant


#tag EndWindowCode

#tag Events StartButton
	#tag Event
		Sub Action()
		  dim temp(-1) As string
		  dim i,j as integer
		  
		  for i = 1 to 100
		    temp.Append mid(tiles,i,1)
		  next
		  
		  temp.Shuffle
		  for i=0 to 9
		    for j=0 to 9
		      grid(i,j) = temp.Pop
		    next
		  next
		  score = 0
		  gameOver = false
		  updateLabels
		  updateHL
		  ClearButton.Caption = "Clear"
		  me.Enabled = false
		  
		End Sub
	#tag EndEvent
#tag EndEvents
#tag Events ClearButton
	#tag Event
		Sub Action()
		  clearAction
		  
		End Sub
	#tag EndEvent
#tag EndEvents
#tag ViewBehavior
	#tag ViewProperty
		Name="Name"
		Visible=true
		Group="ID"
		Type="String"
		EditorType="String"
	#tag EndViewProperty
	#tag ViewProperty
		Name="Interfaces"
		Visible=true
		Group="ID"
		Type="String"
		EditorType="String"
	#tag EndViewProperty
	#tag ViewProperty
		Name="Super"
		Visible=true
		Group="ID"
		Type="String"
		EditorType="String"
	#tag EndViewProperty
	#tag ViewProperty
		Name="Width"
		Visible=true
		Group="Size"
		InitialValue="600"
		Type="Integer"
	#tag EndViewProperty
	#tag ViewProperty
		Name="Height"
		Visible=true
		Group="Size"
		InitialValue="400"
		Type="Integer"
	#tag EndViewProperty
	#tag ViewProperty
		Name="MinWidth"
		Visible=true
		Group="Size"
		InitialValue="64"
		Type="Integer"
	#tag EndViewProperty
	#tag ViewProperty
		Name="MinHeight"
		Visible=true
		Group="Size"
		InitialValue="64"
		Type="Integer"
	#tag EndViewProperty
	#tag ViewProperty
		Name="MaxWidth"
		Visible=true
		Group="Size"
		InitialValue="32000"
		Type="Integer"
	#tag EndViewProperty
	#tag ViewProperty
		Name="MaxHeight"
		Visible=true
		Group="Size"
		InitialValue="32000"
		Type="Integer"
	#tag EndViewProperty
	#tag ViewProperty
		Name="Frame"
		Visible=true
		Group="Frame"
		InitialValue="0"
		Type="Integer"
		EditorType="Enum"
		#tag EnumValues
			"0 - Document"
			"1 - Movable Modal"
			"2 - Modal Dialog"
			"3 - Floating Window"
			"4 - Plain Box"
			"5 - Shadowed Box"
			"6 - Rounded Window"
			"7 - Global Floating Window"
			"8 - Sheet Window"
			"9 - Metal Window"
			"11 - Modeless Dialog"
		#tag EndEnumValues
	#tag EndViewProperty
	#tag ViewProperty
		Name="Title"
		Visible=true
		Group="Frame"
		InitialValue="Untitled"
		Type="String"
	#tag EndViewProperty
	#tag ViewProperty
		Name="CloseButton"
		Visible=true
		Group="Frame"
		InitialValue="True"
		Type="Boolean"
		EditorType="Boolean"
	#tag EndViewProperty
	#tag ViewProperty
		Name="Resizeable"
		Visible=true
		Group="Frame"
		InitialValue="True"
		Type="Boolean"
		EditorType="Boolean"
	#tag EndViewProperty
	#tag ViewProperty
		Name="MaximizeButton"
		Visible=true
		Group="Frame"
		InitialValue="True"
		Type="Boolean"
		EditorType="Boolean"
	#tag EndViewProperty
	#tag ViewProperty
		Name="MinimizeButton"
		Visible=true
		Group="Frame"
		InitialValue="True"
		Type="Boolean"
		EditorType="Boolean"
	#tag EndViewProperty
	#tag ViewProperty
		Name="FullScreenButton"
		Visible=true
		Group="Frame"
		InitialValue="False"
		Type="Boolean"
		EditorType="Boolean"
	#tag EndViewProperty
	#tag ViewProperty
		Name="Composite"
		Group="OS X (Carbon)"
		InitialValue="False"
		Type="Boolean"
	#tag EndViewProperty
	#tag ViewProperty
		Name="MacProcID"
		Group="OS X (Carbon)"
		InitialValue="0"
		Type="Integer"
	#tag EndViewProperty
	#tag ViewProperty
		Name="FullScreen"
		Group="Behavior"
		InitialValue="False"
		Type="Boolean"
		EditorType="Boolean"
	#tag EndViewProperty
	#tag ViewProperty
		Name="ImplicitInstance"
		Visible=true
		Group="Behavior"
		InitialValue="True"
		Type="Boolean"
		EditorType="Boolean"
	#tag EndViewProperty
	#tag ViewProperty
		Name="LiveResize"
		Group="Behavior"
		InitialValue="True"
		Type="Boolean"
		EditorType="Boolean"
	#tag EndViewProperty
	#tag ViewProperty
		Name="Placement"
		Visible=true
		Group="Behavior"
		InitialValue="0"
		Type="Integer"
		EditorType="Enum"
		#tag EnumValues
			"0 - Default"
			"1 - Parent Window"
			"2 - Main Screen"
			"3 - Parent Window Screen"
			"4 - Stagger"
		#tag EndEnumValues
	#tag EndViewProperty
	#tag ViewProperty
		Name="Visible"
		Visible=true
		Group="Behavior"
		InitialValue="True"
		Type="Boolean"
		EditorType="Boolean"
	#tag EndViewProperty
	#tag ViewProperty
		Name="HasBackColor"
		Visible=true
		Group="Background"
		InitialValue="False"
		Type="Boolean"
	#tag EndViewProperty
	#tag ViewProperty
		Name="BackColor"
		Visible=true
		Group="Background"
		InitialValue="&hFFFFFF"
		Type="Color"
	#tag EndViewProperty
	#tag ViewProperty
		Name="Backdrop"
		Visible=true
		Group="Background"
		Type="Picture"
		EditorType="Picture"
	#tag EndViewProperty
	#tag ViewProperty
		Name="MenuBar"
		Visible=true
		Group="Menus"
		Type="MenuBar"
		EditorType="MenuBar"
	#tag EndViewProperty
	#tag ViewProperty
		Name="MenuBarVisible"
		Visible=true
		Group="Deprecated"
		InitialValue="True"
		Type="Boolean"
		EditorType="Boolean"
	#tag EndViewProperty
	#tag ViewProperty
		Name="unplaced"
		Group="Behavior"
		Type="String"
		EditorType="MultiLineEditor"
	#tag EndViewProperty
	#tag ViewProperty
		Name="mdx"
		Group="Behavior"
		Type="Integer"
	#tag EndViewProperty
	#tag ViewProperty
		Name="mdy"
		Group="Behavior"
		Type="Integer"
	#tag EndViewProperty
	#tag ViewProperty
		Name="mux"
		Group="Behavior"
		Type="Integer"
	#tag EndViewProperty
	#tag ViewProperty
		Name="muy"
		Group="Behavior"
		Type="Integer"
	#tag EndViewProperty
	#tag ViewProperty
		Name="longword"
		Group="Behavior"
		Type="String"
		EditorType="MultiLineEditor"
	#tag EndViewProperty
	#tag ViewProperty
		Name="score"
		Group="Behavior"
		Type="Integer"
	#tag EndViewProperty
	#tag ViewProperty
		Name="gameOver"
		Group="Behavior"
		Type="Boolean"
	#tag EndViewProperty
	#tag ViewProperty
		Name="letters"
		Group="Behavior"
		Type="Integer"
	#tag EndViewProperty
	#tag ViewProperty
		Name="highscore"
		Group="Behavior"
		Type="Integer"
	#tag EndViewProperty
#tag EndViewBehavior
