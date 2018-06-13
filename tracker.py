import socket, sys, threading, json,time,optparse,os

def validate_ip(s):
    a = s.split('.')
    if len(a) != 4:
        return False
    for x in a:
        if not x.isdigit():
            return False
        i = int(x)
        if i < 0 or i > 255:
            return False
    return True

def validate_port(x):
    if not x.isdigit():
        return False
    i = int(x)
    if i < 0 or i > 65535:
            return False
    return True

class Tracker(threading.Thread):
    def __init__(self, port, host='0.0.0.0'):
        threading.Thread.__init__(self)
        self.port = port
        self.host = host
        self.BUFFER_SIZE = 8192
        self.server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.users = {} # current connections  self.users[(ip,port)] = {'exptime':}
        self.files = {} #{'ip':,'port':,'mtime':}
        self.lock = threading.Lock()
        try:
            #YOUR CODE
            #Bind to address and port
            self.server.bind((self.host,self.port))#i dont know why i need double parentheses
        except socket.error:
            print('Bind failed %s' % (socket.error))
            sys.exit()
        #YOUR CODE
        #listen for connections
        self.server.listen(5)

            

    def check_user(self):
        #YOUR CODE
        #checking users are alive
        #print self.users
        #print self.files
        self.lock.acquire()
        for i in self.users.keys():
            if (not self.users):
                pass
            elif self.users[i]['exptime']<=time.time():
                for j in self.files.keys():
                    #if self.users[i][1]==self.files[j]['port']:
                    if i[1]==self.files[j]['port']:
                        del self.files[j]
                del self.users[i]
            
        self.lock.release()

           # print "exptime"+str(self.users['exptime'])
        threading.Timer(5,self.check_user).start()
    #Ensure sockets are closed on disconnect
    def exit(self):
        self.server.close()

    def run(self):
        print('Waiting for connections on port %s' % (self.port))
        #threading.Timer(5,self.check_user).start()
        while True:
            #YOUR CODE
            #accept incoming connection and create a thread for receiving messages from FileSynchronizer
            conn, addr = self.server.accept()
            threading.Thread(target=self.proces_messages, args=(conn, addr)).start()
            threading.Thread(target=self.check_user).start()

    def proces_messages(self, conn, addr):
        conn.settimeout(180.0)
        print 'Client connected with ' + addr[0] + ':' + str(addr[1])
        while True:
            #recive data

            data = ''
            while True:                
                part = conn.recv(self.BUFFER_SIZE)
                data =data+ part
                #print ('Reveived: '+data)
                if len(part) < self.BUFFER_SIZE:
                    break
            #YOUR CODE
            # check if the received data is a json string and load the json string

            data_dic = json.loads(data)
            clientPort=data_dic['port']
            #print data_dic
            self.lock.acquire()
            #print 'get process lock'
            if 'files' in data_dic:
                if (data_dic['files']):
                    filename=data_dic['files'][0]['name']
                    #python2 can't recognise unicode. after doing this,
                    #can get get rid of the prefix u'
                    self.files[filename]= {'ip':addr[0]}
                    self.files[filename]['ip']= addr[0]
                    self.files[filename]['port']= data_dic['port']
                    self.files[filename]['mtime']= data_dic['files'][0]['mtime']
                    #print 'SELF FILES content '
                    #print self.files
                
                #self.users[addr[0],self.files[filename]['port']]={'exptime':(time.time()+180)}
            #else:
            #print 'Client port'+str(clientPort)+'accesing'
            self.users[addr[0],clientPort]={'exptime':(time.time()+50)}
            #print self.users[addr[0],clientPort]
            #print self.users
            # sync and send files json data
            #print json.dumps(self.files)
            self.lock.release()
            #print 'release process lock'
            conn.sendall(json.dumps(self.files))
        

        conn.close() # Close
        
if __name__ == '__main__':
    parser = optparse.OptionParser(usage="%prog ServerIP ServerPort")
    options, args = parser.parse_args()
    if len(args) < 1:
        parser.error("No ServerIP and ServerPort")
    elif len(args) < 2:
        parser.error("No  ServerIP or ServerPort")
    else:
        if validate_ip(args[0]) and validate_port(args[1]):
            server_ip = args[0]
            server_port = int(args[1])
        else:
            parser.error("Invalid ServerIP or ServerPort")
    tracker = Tracker(server_port,server_ip)
    tracker.start()
