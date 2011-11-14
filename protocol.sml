signature PROTOCOL =
sig
    type origin
    type message
    type ircsock

    val nick : string ref
    val username : string ref
    val hostname : string ref
    val realname : string ref
    val server : string ref
    val channel : string ref

    val prefToString : origin -> string
    val paramsToString : string list -> string list
    val toString : message -> string
    val message : string * string list -> message
    val send : ircsock * message -> unit

    val installHandler : string -> (((message -> unit) * message) -> unit) -> unit
    val getHandler : string -> (((message -> unit) * message) -> unit) 

    val eventHandler : ircsock -> unit

    val connect : string * int -> ircsock
    val register : ircsock -> unit

end

structure Protocol :> PROTOCOL =
struct
    datatype origin = datatype IrcParser.origin
    type message = IrcParser.message

    type ircsock = (INetSock.inet,Socket.active Socket.stream) Socket.sock

    val nick : string ref = ref "smlirc"
    val username : string ref = ref "smlirc"
    val hostname : string ref = ref "127.0.0.1"
    val realname : string ref = ref "Standard ML IRC Client Library"
    val server : string ref = ref "localhost"
    val channel : string ref = ref "#smlirc"

    val readBuf : string ref = ref ""

    val handlers : (string * (((message -> unit) * message) -> unit)) list ref = ref []
   
    fun prefToString (NoOrigin) = ""
      | prefToString (User (n,NONE,NONE)) = ":" ^ n ^ " "
      | prefToString (User (n,SOME u,NONE)) = ":" ^ n ^ "!" ^ u ^ " "
      | prefToString (User (n,NONE,SOME h)) = ":" ^ n ^ "@" ^ h ^ " "
      | prefToString (User (n,SOME u,SOME h)) = ":" ^ n ^ "!" ^ u ^"@" ^ h ^" "
      | prefToString (Server s) = ":" ^ s ^ " "

    fun paramsToString [] = []
      | paramsToString [h] = [":" ^ h]
      | paramsToString l = 
        List.rev ((fn (h::t) => (":" ^ h) :: t | [] => []) (List.rev l))

    fun toString ({prefix=prefix,command=command,params=params}) =
        prefToString prefix ^ command ^ " " ^
            String.concatWith " " (paramsToString params)

    
    fun message (cmd,params) = 
        {prefix=NoOrigin,command=cmd,params=params}

    fun send' (sock,s) =
        Socket.sendVec (sock, Word8VectorSlice.full (Byte.stringToBytes s))

    fun send (sock,m) =
        (send' (sock, toString m ^ "\n"); ())

    fun recv sock =
        case Socket.recvVecNB (sock, 4096) of
            NONE => ""
          | SOME v => Byte.bytesToString v

    fun pollServer sock =
        readBuf := (!readBuf) ^ recv sock


    fun installHandler evt f =
        handlers := (evt,f) :: (!handlers)

    fun getHandler evt =
        case List.find (fn (e,_) => evt = e) (!handlers) of
            NONE => (fn (s,p) => print ("Unhandled event '" ^ evt ^
                                "': " ^ String.concatWith " " (#params p) ^ "\n"))
          | SOME (e,f) => f

    exception NoEvent

    fun sleep () = OS.Process.sleep (Time.fromMilliseconds 250)

    fun eventHandler s =
    let
        fun takeUntil [] = raise NoEvent
          | takeUntil (#"\n"::t) = [#"\n"]
          | takeUntil (h::t) = h :: takeUntil t

        val line = String.implode (takeUntil (String.explode (!readBuf)))

        val _ = readBuf := String.extract (!readBuf, size line, NONE)

        val m = IrcParser.kupeg_start line
        val _ = IrcParser.kupeg_reset ()

        val h = getHandler (#command m)

        val _ = h (fn x => send (s,x), m)
    in
        (sleep (); eventHandler s)
    end handle NoEvent => (sleep (); pollServer s; eventHandler s)
             | _ => (print "[encountered exception.]\n"; sleep (); eventHandler s)

    fun connect (server,port) =
    let
        val addr = 
            case NetHostDB.getByName server of
                NONE => raise Fail "Server not found"
              | SOME s => INetSock.toAddr (NetHostDB.addr s,port)

        val s = INetSock.TCP.socket ()
        val _ = Socket.connect (s,addr)
    in
        s
    end

    fun register sock =
        (send (sock, message ("NICK", [!nick]))
        ;send (sock, message ("USER", 
             [!username, !hostname, !server, !realname]))
        )

    val _ = installHandler "PING" (fn (send,l) => send (message ("PONG", #params l)))
    val _ = installHandler "376" (fn (send,l) => send (message ("JOIN", [!channel])))
    val _ = installHandler "PRIVMSG" (fn (send,l) =>
                if not (String.isSubstring (!nick) (String.concatWith " " (#params l))) then () else
                    send (message ("PRIVMSG", [!channel, "You called?"])))
end

val s : Protocol.ircsock = Protocol.connect (!Protocol.server, 6667)
val _ = Protocol.register s
val _ = Protocol.send (s, Protocol.message ("JOIN", [!Protocol.channel]))
val _ = Protocol.eventHandler s
