Simple remote access.  
The program allows you to connect to a remote PC via a proxy server.  
The system consists of a client application and a proxy server, through which the connection occurs.
If the remote PC is in the same network as you, then the server can be launched on any PC.  
If the remote PC is outside the network, then the server must be launched on a PC with an external IP address.
The desktop image from the remote PC is encrypted using AEC 256, as are the keystrokes on the keyboard. The copied text is also encrypted.    
The program is support transferring files both to and from a remote PC(file transfer does not support encryption).
Audio transmission is not supported. Copying an image is also not supported yet.  
The launched client application has a unique ID that uniquely corresponds to this PC.  
To access a remote PC, you need to know its ID since the common encryption key is calculated based on the ID of the remote and connected PC.  
The client ID that initiates the connection can be whitelisted to allow it to connect without permission, otherwise permission will be required to connect.
The system is designed so that the traffic between clients is as low as possible. Therefore, it is possible to regulate the quality of the remote desktop stream. 
By default, the image quality is set to 5, providing a balance between quality and traffic. The value 0 gives a bad image with a lot of artifacts, but with minimal traffic,  
while the value 9 gives an ideal image, but with a lot of traffic. 
The amount of traffic changes depending on how much the desktop on the remote PC changes, if there are no changes, then no traffic is used.
Usage:  
Run the server application on any pc if they are in the same network, or on a pc with an external ip address.  
Run client applications on your PCs, click the server button and set the IP address of the proxy server. Then press the save button.  
If you have connected to the server, the text field will show the authorization message and your ID will be shown.  
At the bottom of the program, to the left of the connect button, there is a drop-down menu for your white list.  
When you first start it, it is empty, so you need to paste the remote client ID here. In fact, the ID looks like a compressed public key.  
The remote client ID can be seen in the text field on the remote machine as (Your ID .....)
Paste the remote machine's ID into your program in the drop-down list and click the connect button.  
Next, on the remote machine, you need to confirm the connection with the accept button or skip it.
After successful connection you will see the screen of the remote PC. By clicking the add button you can add PC to the white list on the remote PC and connect without confirmation.  
You can also click the add button on your PC to avoid constantly entering the remote client's ID.  
Attention! radiobutton <auto> allow the remote PC connect to yours PC without permission.  
Now you can control the remote PC.  
