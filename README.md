# File-Synchronizer-Python
Develop a simple P2P file sharing application that synchronize files among peers.
• Tracker keeps track of live FileSynchronizer peers and maintains a directory of files, IP
addresses, port numbers of the peer with the file. The tracker server’s IP address (TrackerIP) and
port number (TrackerPort) shall be specified as command line arguments. When contacted by
• FileSynchronizer peers, the tracker sends with the directory using a TCP socket.
The tracker listens to its specified port. Upon accepting a new connection (from a user), a new
thread is spawned. The file information on the peer node will be uploaded to the tracker, along
with the IP address of the peer and FileSynchronizerServerPort, the port for serving file transfer
requests to other peers.
• The peer node periodically sends a keepalive message to the tracker to inform the tracker that it is
online. When the tracker receives the message, it will refresh the state of the peer and also send
the directory info to the peer. Otherwise, if the tracker does not get any keepalive message from
the peer node for 180 seconds, it will remove entries of the files associated with the peer.
• FileSynchronizer communicates with the tracker through a single TCP socket to
1. send an Init message containing names and modified time of its local files as well as
FileSynchronizerServerPort when it first starts
2. send a keepalive message containing FileSynchronizerServerPort every 5 seconds to the
tracker
3. receive the directory information maintained by the tracker
