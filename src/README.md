This is a read me file to document the files in the repository:

*Introdcution:
	This is a web application developed in Elm and Haskell. It is meant to be used by patients who suffer from mental health illnesses. I allows users to insert journal inquiries, play games, stay in touch with people close to them, constantly insert their mood. This allows the developers and psychologists to use the entries and mood entries to track the patients over time to prevent and improve their state.

*Requirements 
	Users need to install Elm on the station they are using. This can be done using this webstie: 
	https://guide.elm-lang.org/install.html#elm-package

*Installation:
	To make the app, make sure you have the repository with the latest changes. Then go inside the "src" folder and type the following command (in terminal):
	"elm-make MultiplePages.elm"
	This will generate an index.html file which if you open you can see the applicatoin. You can double click the html file of use the following command:
	"open index.html"

	The elm packages you need are listed under "elm-package.json".
	Use the following command which will install the packages in the elm-package.json file:
	"elm-package install"

*Files:
	- AngryFace.elm:
		This program implements the Angry face which is in the moods in the journal. 
		It has a "myShapes model" which is the main model that connects the whole face. It is what is called in the main program MultiplePages.elm.
		It also has a "face model" which makes up components of the face such as the ears, eyes, etc..
		There is also the slider model this is the slider that appears it is also a funciton of the 'model.happiness' which is what gradually changes the face as the slider moves. 
		The sizes model is used to slightly increase the size of the face as the slider moves.

	- FrustratedFace.elm:
		This program implements the Frustrated face which is in the moods in the journal. 
		It has a "myShapes model" which is the main model that connects the whole face. It is what is called in the main program MultiplePages.elm.
		It also has a "face model" which makes up components of the face such as the ears, eyes, etc..
		There is also the slider model this is the slider that appears it is also a funciton of the 'model.happiness' which is what gradually changes the face as the slider moves. 
		The sizes model is used to slightly increase the size of the face as the slider moves.

	- HappyFace.elm:
		This program implements the Happy face which is in the moods in the journal. 
		It has a "myShapes model" which is the main model that connects the whole face. It is what is called in the main program MultiplePages.elm.
		It also has a "face model" which makes up components of the face such as the ears, eyes, etc..
		There is also the slider model this is the slider that appears it is also a funciton of the 'model.happiness' which is what gradually changes the face as the slider moves. 
		The sizes model is used to slightly increase the size of the face as the slider moves.

	- HopefulFace.elm:
		This program implements the Hopeful face which is in the moods in the journal. 
		It has a "myShapes model" which is the main model that connects the whole face. It is what is called in the main program MultiplePages.elm.
		It also has a "face model" which makes up components of the face such as the ears, eyes, etc..
		There is also the slider model this is the slider that appears it is also a funciton of the 'model.happiness' which is what gradually changes the face as the slider moves. 
		The sizes model is used to slightly increase the size of the face as the slider moves.

	- SadFace.elm:
		This program implements the Sad face which is in the moods in the journal. 
		It has a "myShapes model" which is the main model that connects the whole face. It is what is called in the main program MultiplePages.elm.
		It also has a "face model" which makes up components of the face such as the ears, eyes, etc..
		There is also the slider model this is the slider that appears it is also a funciton of the 'model.happiness' which is what gradually changes the face as the slider moves. 
		The sizes model is used to slightly increase the size of the face as the slider moves.

	- VibrantFace.elm
		This program implements the Vibrant face which is in the moods in the journal. 
		It has a "myShapes model" which is the main model that connects the whole face. It is what is called in the main program MultiplePages.elm.
		It also has a "face model" which makes up components of the face such as the ears, eyes, etc..
		There is also the slider model this is the slider that appears it is also a funciton of the 'model.happiness' which is what gradually changes the face as the slider moves. 
		The sizes model is used to slightly increase the size of the face as the slider moves.
		This face also has a bounce which increases as the face goes on -> Elevation model		

	- AvatarCreator.elm:
		This is the program that creates the different avatar styles where the user can change hair/skin tone/ etc..

	- CircleOfSix.elm:
		This program provides the circle that has 6 contacts. Each contact has an associated phone number so the user could make calls to those closest to them in just one click.

	- Graph.elm: This creates the graph, reading from the rates of the moods (to be added), and shows the average mood of the user.

	- MultiplePages.elm:
		This is the main program that gets run to produce the application and put all the files together. It has a main unlike most other files which is why when it is run the index.html file has the application content in it.

	- SvgCalendar.elm:
		This file is what has most of what relates to the calendar such as the Day cell (the journal entry). The days of the week, etc.

	- Tetris.elm:
		This is the Tetris game. It is to work like the real tetris game, blocks shift left and right, new block come down. The blocks are not plain, they are shaped like different animals to make the game more visually satisfying and fun. The animal are supposed to pop out when a line is formed.

		
	Backend:

    The backend stores all the information of users on the server such as their journal entries etc.. A registration page and a login page will be avaliable in elm, once a user successfully registered, he/she will be able to have their own account. The backend saves the user's username and password on the server which would futher helps to confirm the identity when the user is trying to login.
