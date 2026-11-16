import json,os,time,urllib.request as U
B=0x88
V=(0,100,320,330,500,900,100000)
NK=(18,33,31,14,-18,-33,-31,-14)
SL=(17,15,-17,-15,1,16,-1,-16)
INF=99999;MD=4;TIME=1400
def pst(pc,i):
 if i&B or not pc:return 0
 r=i>>4;f=i&7
 if pc&8:r=7-r
 t=pc&7
 if t==1:return 0
 if t==2:return(80-(abs(r-3)+abs(f-4))*12)*(-1 if pc&8 else 1)
 if t==3:return(60-(abs(r-3)+abs(f-4))*8)*(-1 if pc&8 else 1)
 if t in(4,5):return(30-(abs(r-3)+abs(f-4))*5)*(-1 if pc&8 else 1)
 return 0
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
def gen(b,s,ep):
 ms=[];own=8 if s<0 else 0
 for sq in range(128):
  if sq&B:continue
  pc=b[sq]
  if not pc or(pc&8)!=own:continue
  t=pc&7
  if t==1:
   d=16 if s>0 else-16;nt=sq+d
   if not nt&B and not b[nt]:
    ms.append((sq,nt,0))
    if sq>>4==(1 if s>0 else 6):
     nt2=sq+d*2
     if not nt2&B and not b[nt2]:ms.append((sq,nt2,0))
   for c in(d-1,d+1):
    nt=sq+c
    if nt&B:continue
    if b[nt]and(b[nt]&8)!=own:ms.append((sq,nt,0))
    elif nt==ep:ms.append((sq,nt,0))
  elif t==2:
   for d in NK:
    nt=sq+d
    if not nt&B and(not b[nt]or(b[nt]&8)!=own):ms.append((sq,nt,0))
  else:
   for d in SL:
    nt=sq+d
    while not nt&B:
     if b[nt]:
      if(b[nt]&8)!=own:ms.append((sq,nt,0))
      break
     ms.append((sq,nt,0))
     if t in(3,4,5):nt+=d
     else:break
 return ms
def ap(m,b):
 f,t,p=m;pc=b[f];b[f]=0;b[t]=(pc&8)|p if p else pc;return pc
def unm(m,b,pc,was):b[m[0]]=pc;b[m[1]]=was
def att(b,sq,en):
 for d in SL:
  nt=sq+d
  while not nt&B:
   p=b[nt]
   if p:
    if(p&8)==en:
     t=p&7
     if t in(3,4,5):return 1
     if t==6 and abs(sq-nt)in(1,15,16,17):return 1
     if t==1 and((p&8 and sq-nt in(15,17))or(not p&8 and nt-sq in(15,17))):return 1
    break
   nt+=d
 for d in NK:
  nt=sq+d
  if not nt&B and b[nt]and(b[nt]&8)==en and b[nt]&7==2:return 1
 return 0
def ksq(b,s):
 own=8 if s<0 else 0
 for i in range(128):
  if not i&B and b[i]and b[i]&7==6 and(b[i]&8)==own:return i
def legal(b,s,ep):
 out=[];en=8 if s>0 else 0
 for m in gen(b,s,ep):
  was=b[m[1]];pc=ap(m,b)
  ks=ksq(b,s)
  if ks is not None and not att(b,ks,en):out.append(m)
  unm(m,b,pc,was)
 return out
def ev(b,s):
 sc=0
 for i in range(128):
  if i&B:continue
  p=b[i]
  if p:
   t=p&7
   if not t:continue
   sc+=V[t]*(-1 if p&8 else 1)+pst(p,i)
 ks=ksq(b,s)
 if ks is not None and att(b,ks,8 if s>0 else 0):sc-=150
 return sc*(1 if s>0 else-1)
T=0
def ab(b,s,ep,al,be,d):
 global T
 if time.time()-T>TIME:d=1
 if d==0:return ev(b,s)
 ms=legal(b,s,ep)
 if not ms:
  ks=ksq(b,s)
  return -90000+d if(ks is not None and att(b,ks,8 if s>0 else 0))else 0
 ms.sort(key=lambda m:-V[b[m[1]]&7]if b[m[1]]else 0)
 for f,t,p in ms:
  pc=b[f];was=b[t];ap((f,t,p),b)
  v=-ab(b,-s,ep if t!=ep else 0,-be,-al,d-1)
  unm((f,t,p),b,pc,was)
  if v>=be:return v
  if v>al:al=v
 return al
def search(b,s,ep):
 global T
 T=time.time()
 r=[]
 for f,t,p in legal(b,s,ep):
  pc=b[f];was=b[t];ap((f,t,p),b)
  v=-ab(b,-s,ep if t!=ep else 0,-INF,INF,MD-1)
  unm((f,t,p),b,pc,was)
  r.append((v,(f,t,p)))
 r.sort(key=lambda x:-x[0])
 return[m for v,m in r]
def uci(f,t,pr):
 return chr(97+f%16)+str(1+f//16)+chr(97+t%16)+str(1+t//16)+(""if not pr else"q"if pr==2 else"r"if pr==3 else"b"if pr==4 else"n")
E=os.environ["MAKEFUN_ENV_URL"]
def P(p,b):
 q=U.Request(E+p,json.dumps(b).encode(),{"content-type":"application/json"})
 return json.load(U.urlopen(q))
while True:
 g=P("/game",{})
 if g["status"]=="done":break
 b,s,ep=parse(g["fen"])
 ms=search(b,s,ep)
 if not ms:continue
 ok=0
 for m in ms:
  try:
   g=P("/move",{"game":g["game"],"move":uci(*m)});ok=1;break
  except:pass
 if not ok:continue
 if g.get("status")=="done":continue
