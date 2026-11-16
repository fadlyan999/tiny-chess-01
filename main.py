import json,os,urllib.request as U
B2=0x88
V=(0,100,320,330,500,900,100000)
NK=(18,33,31,14,-18,-33,-31,-14)
SL=(17,15,-17,-15,1,16,-1,-16)
INF=99999;MD=3;CAP=25000
PR=(0,5,5,0,5,10,50,0)
Q=(-50,-40,-30,-30,-40,-20,0,0,-30,0,10,15,-30,5,15,20,
   -20,-10,-10,-10,-10,0,0,0,-10,0,5,10,-10,5,5,10,
   0,0,0,0,5,10,10,10,-5,0,0,0,-5,0,0,0,
   -20,-10,-10,-5,-10,0,0,0,-10,0,5,5,-5,0,5,5,
   -30,-40,-40,-50,-30,-40,-40,-50,-30,-40,-40,-50,-20,-30,-30,-40)
def pst(pc,i):
 if i&B2 or not pc:return 0
 r=i>>4;f=i&7
 if pc&8:r=7-r
 t=pc&7
 if t==1:return PR[r]*(-1 if pc&8 else 1)
 qr=r if r<4 else 7-r;qf=f if f<4 else 7-f
 return Q[(t-2)*16+qr*4+qf]*(-1 if pc&8 else 1)
def parse(f):
 b=[0]*128;ep=0
 p,cs,cr_s,ep_s,_,_=f.split()
 for r,row in enumerate(p.split("/")):
  fl=0
  for ch in row:
   if ch.isdigit():fl+=int(ch)
   else:
    sq=(7-r)*16+fl;pt='pnbrqk'.index(ch.lower())+1
    b[sq]=pt if ch.isupper() else pt|8;fl+=1
 if ep_s!="-":ep=(int(ep_s[1])-1)*16+(ord(ep_s[0])-97)
 return b,(1 if cs=="w" else -1),ep
def mvv(ms,b):
 r=[]
 for f,t,pr in ms:
  sc=V[b[t]&7]if b[t]else 0
  sc+=(b[f]&7)<<12
  r.append((sc,(f,t,pr)))
 r.sort(key=lambda x:-x[0])
 return[x[1]for x in r]
def gen(b,s,ep):
 ms=[]
 own=8 if s<0 else 0
 for sq in range(128):
  if sq&B2:continue
  pc=b[sq]
  if not pc or(pc&8)!=own:continue
  t=pc&7
  if t==1:
   d=16 if s>0 else-16;nt=sq+d
   if not nt&B2 and not b[nt]:
    ms.append((sq,nt,0))
    if sq>>4==(1 if s>0 else 6):
     nt2=sq+d*2
     if not nt2&B2 and not b[nt2]:ms.append((sq,nt2,0))
   for c in(d-1,d+1):
    nt=sq+c
    if nt&B2:continue
    if b[nt]and(b[nt]&8)!=own:ms.append((sq,nt,0))
    elif nt==ep:ms.append((sq,nt,0))
  elif t==2:
   for d in NK:
    nt=sq+d
    if not nt&B2 and(not b[nt]or(b[nt]&8)!=own):ms.append((sq,nt,0))
  else:
   for d in SL:
    nt=sq+d
    while not nt&B2:
     if b[nt]:
      if(b[nt]&8)!=own:ms.append((sq,nt,0))
      break
     ms.append((sq,nt,0))
     if t in(3,4,5):nt+=d
     else:break
 return mvv(ms,b)
def ap(m,b):
 f,t,p=m;pc=b[f];b[f]=0;b[t]=(pc&8)|p if p else pc
 return pc
def unm(m,b,pc,was):b[m[0]]=pc;b[m[1]]=was
def see(b,m):
 f,t,pr=m;p=b[f];oc=b[t];b[f]=0
 if pr:oc=5
 elif p&7==1 and not oc and abs(t-f)in(1,15,17):oc=1;b[t]=0
 sc=V[oc&7]if oc else 0;v=0
 for c in NK:
  nt=t+c
  if nt&B2 or not b[nt]or(b[nt]&8)==(p&8):continue
  v=max(v,V[b[nt]&7]-see(b,(t,nt,0)));break
 b[f]=p;b[t]=oc
 return v
def ev(b,s):
 sc=0
 for i in range(128):
  if i&B2:continue
  p=b[i]
  if p:
   t=p&7
   if not t:continue
   sc+=V[t]*(-1 if p&8 else 1)
   sc+=pst(p,i)
 return sc*(1 if s>0 else-1)
N=0
def ab(b,s,ep,al,be,d):
 global N
 N+=1
 if d==0 or N>CAP:return ev(b,s)
 ms=gen(b,s,ep)
 if not ms:return ev(b,s)
 ms.sort(key=lambda m:-see(b,m))
 for f,t,p in ms:
  pc=b[f];was=b[t];ap((f,t,p),b)
  v=-ab(b,-s,ep if t!=ep else 0,-be,-al,d-1)
  unm((f,t,p),b,pc,was)
  if v>=be:return v
  if v>al:al=v
 return al
def search(b,s,ep):
 global N
 N=0
 r=[]
 for f,t,p in gen(b,s,ep):
  pc=b[f];was=b[t];ap((f,t,p),b)
  v=ev(b,-s)
  unm((f,t,p),b,pc,was)
  r.append((v,(f,t,p)))
 r.sort(key=lambda x:-x[0])
 best=[]
 for v,(f,t,p)in r[:6]:
  pc=b[f];was=b[t];ap((f,t,p),b)
  v=-ab(b,-s,ep if t!=ep else 0,-INF,INF,MD)
  unm((f,t,p),b,pc,was)
  best.append((v,(f,t,p)))
 best.sort(key=lambda x:-x[0])
 return[m for v,m in best]
def uci(f,t,pr):
 sf=chr(97+f%16)+str(1+f//16);st=chr(97+t%16)+str(1+t//16)
 return sf+st+(""if not pr else"q"if pr==2 else"r"if pr==3 else"b"if pr==4 else"n")
E=os.environ["MAKEFUN_ENV_URL"]
def P(p,b):
 q=U.Request(E+p,json.dumps(b).encode(),{"content-type":"application/json"})
 return json.load(U.urlopen(q))
while True:
 g=P("/game",{})
 if g["status"]=="done":break
 b,s,ep=parse(g["fen"])
 ms=gen(b,s,ep)
 if not ms:break
 ok=0
 for m in search(b,s,ep):
  try:
   g=P("/move",{"game":g["game"],"move":uci(*m)});ok=1;break
  except Exception:pass
 if not ok:break
 if g.get("status")=="done":break
