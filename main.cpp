#include <cstdio>
#include <cstring>
#include <string>
#include <iostream>
using namespace std;

struct StrIntMap {
    struct Node{ string key; int val; int next; };
    int N; int M; int *head; Node *nodes; int used;
    StrIntMap(int cap=1<<14){ N=cap; M=cap*2; head=new int[M]; for(int i=0;i<M;i++) head[i]=-1; nodes=new Node[N]; used=0; }
    ~StrIntMap(){ delete[] head; delete[] nodes; }
    inline unsigned h(const string &s){ unsigned x=2166136261u; for(char c: s){ x^=(unsigned char)c; x*=16777619u; } return x; }
    int get(const string &k){ unsigned idx=h(k)%M; for(int e=head[idx]; e!=-1; e=nodes[e].next){ if(nodes[e].key==k) return nodes[e].val; } return -1; }
    bool put(const string &k,int v){ if(used>=N) return false; unsigned idx=h(k)%M; for(int e=head[idx]; e!=-1; e=nodes[e].next){ if(nodes[e].key==k){ nodes[e].val=v; return true; } }
        nodes[used]={k,v,head[idx]}; head[idx]=used++; return true; }
    bool del(const string &k){ unsigned idx=h(k)%M; int prev=-1; for(int e=head[idx]; e!=-1; prev=e,e=nodes[e].next){ if(nodes[e].key==k){ if(prev==-1) head[idx]=nodes[e].next; else nodes[prev].next=nodes[e].next; return true;} } return false; }
};

inline void parseDate(const string &d, int &m,int &day){ m=(d[0]-'0')*10+(d[1]-'0'); day=(d[3]-'0')*10+(d[4]-'0'); }
inline void parseTime(const string &t, int &h,int &mi){ h=(t[0]-'0')*10+(t[1]-'0'); mi=(t[3]-'0')*10+(t[4]-'0'); }
inline int dateToAbs(int m,int d){ int days=(m==6?0:(m==7?30:61)); return days + (d-1); }
inline void absToDate(int a,int &m,int &d){ if(a<30){ m=6; d=a+1; } else if(a<61){ m=7; d=a-29; } else { m=8; d=a-60; } }
inline string fmtDate(int m,int d){ char buf[16]; sprintf(buf,"%02d-%02d",m,d); return string(buf); }
inline string fmtTime(int h,int mi){ char buf[16]; sprintf(buf,"%02d:%02d",h,mi); return string(buf); }
inline int addMinutes(int day,int h,int mi,int add){ int total=h*60+mi+add; int nd=day + total/1440; total%=1440; if(total<0){ total+=1440; nd--; } h=total/60; mi=total%60; return (nd<<16)|(h<<8)|mi; }

static int USER_CAP=1024;
static int TRAIN_CAP=256;

struct Order{ string status; string trainID, from, to; int depDay,depH,depMi; int arrDay,arrH,arrMi; int price; int num; };

struct User{ string username,password,name,mail; int priv; bool online; Order *orders; int ocnt; int ocap; };

struct Train{
    string id; int stationNum; int seatNum; char type; bool released;
    string *station; int *price; int *travel; int *stop;
    int saleStartM,saleStartD,saleEndM,saleEndD; int startHr,startMin;
    int saleStartAbs,saleEndAbs; int segN; int saleDays; int *seat;
};

User *users; int userCount=0; StrIntMap userIndex(1<<16);
Train *trains; int trainCount=0; StrIntMap trainIndex(1<<14);

inline void ensureUserCap(){ if(userCount<USER_CAP) return; int newCap=USER_CAP*2; User *nw=new User[newCap]; for(int i=0;i<userCount;i++) nw[i]=users[i]; delete[] users; users=nw; USER_CAP=newCap; }
inline void ensureTrainCap(){ if(trainCount<TRAIN_CAP) return; int newCap=TRAIN_CAP*2; Train *nw=new Train[newCap]; for(int i=0;i<trainCount;i++) nw[i]=trains[i]; delete[] trains; trains=nw; TRAIN_CAP=newCap; }

inline void splitPipeAlloc(const string &s, string* &arr, int &cnt){ cnt=1; for(char c: s) if(c=='|') cnt++; arr = new string[cnt]; int idx=0; string cur; for(char c: s){ if(c=='|'){ arr[idx++]=cur; cur.clear(); } else cur.push_back(c);} arr[idx++]=cur; }
inline void splitTokens(const string &line, string *&arr, int &cnt){ cnt=0; int n=line.size(); string cur; for(int i=0;i<n;i++){ char c=line[i]; if(c==' '||c=='\t'||c=='\r'||c=='\n'){ if(cur.size()){ cnt++; cur.clear(); } } else cur.push_back(c); } if(cur.size()) cnt++; arr=new string[cnt]; int idx=0; cur.clear(); for(int i=0;i<n;i++){ char c=line[i]; if(c==' '||c=='\t'||c=='\r'||c=='\n'){ if(cur.size()){ arr[idx++]=cur; cur.clear(); } } else cur.push_back(c); } if(cur.size()) arr[idx++]=cur; }
inline bool getArg(string *tok,int cnt,const string &key,string &val){ for(int i=1;i<cnt-1;i++){ if(tok[i]==key){ val=tok[i+1]; return true; } } return false; }

int add_user(const string &c,const string &u,const string &p,const string &n,const string &m,int g){
    if(userCount==0){ if(userIndex.get(u)!=-1) return -1; users[userCount]={u,p,n,m,10,true,nullptr,0,0}; userIndex.put(u,userCount); userCount++; ensureUserCap(); return 0; }
    int ci=userIndex.get(c); if(ci==-1||!users[ci].online) return -1; if(userIndex.get(u)!=-1) return -1; if(g>=users[ci].priv) return -1;
    users[userCount]={u,p,n,m,g,false,nullptr,0,0}; userIndex.put(u,userCount); userCount++; ensureUserCap(); return 0;
}

int login(const string &u,const string &p){ int i=userIndex.get(u); if(i==-1) return -1; if(users[i].online) return -1; if(users[i].password!=p) return -1; users[i].online=true; return 0; }
int logout(const string &u){ int i=userIndex.get(u); if(i==-1) return -1; if(!users[i].online) return -1; users[i].online=false; return 0; }

int query_profile(const string &c,const string &u,string &out){ int ci=userIndex.get(c); int ui=userIndex.get(u); if(ci==-1||ui==-1||!users[ci].online) return -1; if(!(users[ci].priv>users[ui].priv || ci==ui)) return -1; char buf[256]; sprintf(buf,"%s %s %s %d", users[ui].username.c_str(), users[ui].name.c_str(), users[ui].mail.c_str(), users[ui].priv); out=buf; return 0; }
int modify_profile(const string &c,const string &u,const string *p,const string *n,const string *m,const int *g,string &out){ int ci=userIndex.get(c); int ui=userIndex.get(u); if(ci==-1||ui==-1||!users[ci].online) return -1; if(!(users[ci].priv>users[ui].priv || ci==ui)) return -1; if(g && *g>=users[ci].priv) return -1; if(p) users[ui].password=*p; if(n) users[ui].name=*n; if(m) users[ui].mail=*m; if(g) users[ui].priv=*g; char buf[256]; sprintf(buf,"%s %s %s %d", users[ui].username.c_str(), users[ui].name.c_str(), users[ui].mail.c_str(), users[ui].priv); out=buf; return 0; }

int add_train(const string &id,int n,int seat,const string &stationsStr,const string &pricesStr,const string &startT,const string &travStr,const string &stopStr,const string &saleStr,char type){ if(trainIndex.get(id)!=-1) return -1; Train &t=trains[trainCount]; t.id=id; t.stationNum=n; t.seatNum=seat; t.type=type; t.released=false; t.station=new string[n]; t.price=new int[n-1]; t.travel=new int[n-1]; t.stop=(n>=3? new int[n-2]: nullptr); t.seat=nullptr;
    string *stations; int sc; splitPipeAlloc(stationsStr, stations, sc); for(int i=0;i<n;i++) t.station[i]=stations[i]; delete[] stations;
    string *prices; int pc; splitPipeAlloc(pricesStr, prices, pc); for(int i=0;i<n-1;i++) t.price[i]=stoi(prices[i]); delete[] prices;
    string *trav; int tc; splitPipeAlloc(travStr, trav, tc); for(int i=0;i<n-1;i++) t.travel[i]=stoi(trav[i]); delete[] trav;
    if(n>=3){ string *stops; int oc; splitPipeAlloc(stopStr, stops, oc); for(int i=0;i<n-2;i++) t.stop[i]=stoi(stops[i]); delete[] stops; }
    int h,mi; parseTime(startT,h,mi); t.startHr=h; t.startMin=mi; string *sale; int dc; splitPipeAlloc(saleStr, sale, dc); int sm,sd,em,ed; parseDate(sale[0],sm,sd); parseDate(sale[1],em,ed); t.saleStartM=sm; t.saleStartD=sd; t.saleEndM=em; t.saleEndD=ed; delete[] sale; t.saleStartAbs=dateToAbs(t.saleStartM,t.saleStartD); t.saleEndAbs=dateToAbs(t.saleEndM,t.saleEndD); t.segN=n-1; t.saleDays=t.saleEndAbs - t.saleStartAbs + 1;
    trainIndex.put(id,trainCount); trainCount++; ensureTrainCap(); return 0; }

int release_train(const string &id){ int i=trainIndex.get(id); if(i==-1) return -1; if(trains[i].released) return -1; Train &t=trains[i]; t.released=true; t.seat = new int[t.saleDays * t.segN]; for(int d=0; d<t.saleDays; ++d){ for(int s=0;s<t.segN;s++) t.seat[d*t.segN+s]=t.seatNum; } return 0; }
int delete_train(const string &id){ int i=trainIndex.get(id); if(i==-1) return -1; if(trains[i].released) return -1; trainIndex.del(id); return 0; }

int query_train(const string &id,const string &date){ int i=trainIndex.get(id); if(i==-1) return -1; Train &t=trains[i]; int dm,dd; parseDate(date,dm,dd); int day0=dateToAbs(dm,dd);
    printf("%s %c\n", t.id.c_str(), t.type);
    int curDay=day0; int h=t.startHr, mi=t.startMin; for(int s=0;s<t.stationNum;s++){
        int arrDay=-1, arrH=-1, arrMi=-1; int depDay=-1, depH=-1, depMi=-1; int priceCum=0; if(s==0){ depDay=curDay; depH=h; depMi=mi; }
        else{ int pack=addMinutes(curDay, h, mi, t.travel[s-1]); arrDay=pack>>16; arrH=(pack>>8)&255; arrMi=pack&255; if(s!=t.stationNum-1){ int pack2=addMinutes(arrDay, arrH, arrMi, t.stop[s-1]); depDay=pack2>>16; depH=(pack2>>8)&255; depMi=pack2&255; curDay=depDay; h=depH; mi=depMi; } }
        for(int k=0;k<s;k++) priceCum+=t.price[k]; string arrStr, depStr; if(s==0){ arrStr = "xx-xx xx:xx"; } else { int m,d; absToDate(arrDay,m,d); arrStr = fmtDate(m,d)+" "+fmtTime(arrH,arrMi); }
        if(s==t.stationNum-1){ depStr = "xx-xx xx:xx"; } else { int m,d; absToDate(depDay,m,d); depStr = fmtDate(m,d)+" "+fmtTime(depH,depMi); }
        if(s==t.stationNum-1){ printf("%s %s -> %s %d x\n", t.station[s].c_str(), arrStr.c_str(), depStr.c_str(), priceCum); }
        else{ int seatLeft=t.seatNum; printf("%s %s -> %s %d %d\n", t.station[s].c_str(), arrStr.c_str(), depStr.c_str(), priceCum, seatLeft); }
    }
    return 0;
}

struct TicketAns{ string tid, from, to; int depDay, depH, depMi; int arrDay, arrH, arrMi; int price; int seat; int rideMin; };
void sortBy(TicketAns *arr,int n,bool byTime){ for(int i=0;i<n;i++){ int best=i; for(int j=i+1;j<n;j++){ if(byTime){ if(arr[j].rideMin<arr[best].rideMin || (arr[j].rideMin==arr[best].rideMin && arr[j].tid < arr[best].tid)) best=j; } else { if(arr[j].price<arr[best].price || (arr[j].price==arr[best].price && arr[j].tid < arr[best].tid)) best=j; } } if(best!=i){ TicketAns tmp=arr[i]; arr[i]=arr[best]; arr[best]=tmp; } } }

int query_ticket(const string &from,const string &to,const string &date,const string &pref){ int qM,qD; parseDate(date,qM,qD); int qAbs=dateToAbs(qM,qD); TicketAns *res=new TicketAns[TRAIN_CAP]; int cnt=0; for(int ti=0; ti<trainCount; ++ti){ Train &t=trains[ti]; if(!t.released) continue; int si=-1, sj=-1; for(int i=0;i<t.stationNum;i++){ if(t.station[i]==from) { si=i; break; } } if(si==-1) continue; for(int j=si+1;j<t.stationNum;j++){ if(t.station[j]==to){ sj=j; break; } } if(sj==-1) continue; int offsetMin=0; for(int k=0;k<si;k++){ offsetMin += t.travel[k]; if(k<=t.stationNum-2-1) offsetMin += t.stop[k]; }
        int offsetDay = offsetMin/1440; int baseStartAbs = qAbs - offsetDay; if(baseStartAbs<t.saleStartAbs || baseStartAbs>t.saleEndAbs) continue;
        int startTotal=t.startHr*60+t.startMin; int depTotal=(startTotal + offsetMin)%1440; int depDay=qAbs; int depH=depTotal/60, depMi=depTotal%60;
        int rideMin=0; for(int k=si;k<sj;k++) rideMin+=t.travel[k]; int arrPack = addMinutes(depDay, depH, depMi, rideMin); int arrDay=arrPack>>16, arrH=(arrPack>>8)&255, arrMi=arrPack&255;
        int price=0; for(int k=si;k<sj;k++) price+=t.price[k]; int dayIdx=baseStartAbs - t.saleStartAbs; int seatLeft=t.seatNum; if(t.seat){ for(int k=si;k<sj;k++){ int v=t.seat[dayIdx*t.segN + k]; if(v<seatLeft) seatLeft=v; } }
        res[cnt]={t.id, t.station[si], t.station[sj], depDay, depH, depMi, arrDay, arrH, arrMi, price, seatLeft, rideMin}; cnt++; }
    bool byTime = (pref=="time"); sortBy(res,cnt,byTime); printf("%d\n", cnt); for(int i=0;i<cnt;i++){ int fm,fd, tm,td; absToDate(res[i].depDay,fm,fd); absToDate(res[i].arrDay,tm,td); string depStr = fmtDate(fm,fd)+" "+fmtTime(res[i].depH,res[i].depMi); string arrStr = fmtDate(tm,td)+" "+fmtTime(res[i].arrH,res[i].arrMi); printf("%s %s %s -> %s %s %d %d\n", res[i].tid.c_str(), res[i].from.c_str(), depStr.c_str(), res[i].to.c_str(), arrStr.c_str(), res[i].price, res[i].seat); }
    delete[] res; return 0; }

int findStation(Train &t,const string &name){ for(int i=0;i<t.stationNum;i++) if(t.station[i]==name) return i; return -1; }

int buy_ticket(const string &uname,const string &tid,const string &date,int num,const string &from,const string &to,bool qflag){ int ui=userIndex.get(uname); if(ui==-1||!users[ui].online) return -1; int ti=trainIndex.get(tid); if(ti==-1) return -1; Train &t=trains[ti]; if(!t.released) return -1; if(num<=0 || num>t.seatNum) return -1; int si=findStation(t,from); int sj=findStation(t,to); if(si==-1||sj==-1||si>=sj) return -1; int qM,qD; parseDate(date,qM,qD); int qAbs=dateToAbs(qM,qD); int offsetMin=0; for(int k=0;k<si;k++){ offsetMin+=t.travel[k]; if(k<=t.stationNum-2-1) offsetMin+=t.stop[k]; } int offsetDay=offsetMin/1440; int baseStartAbs=qAbs - offsetDay; if(baseStartAbs<t.saleStartAbs || baseStartAbs>t.saleEndAbs) return -1; int dayIdx=baseStartAbs - t.saleStartAbs; int seatLeft=t.seatNum; for(int k=si;k<sj;k++){ int v=t.seat[dayIdx*t.segN + k]; if(v<seatLeft) seatLeft=v; }
    int startTotal=t.startHr*60+t.startMin; int depTotal=(startTotal + offsetMin)%1440; int depDay=qAbs; int depH=depTotal/60, depMi=depTotal%60; int rideMin=0; for(int k=si;k<sj;k++) rideMin+=t.travel[k]; int arrPack=addMinutes(depDay,depH,depMi,rideMin); int arrDay=arrPack>>16, arrH=(arrPack>>8)&255, arrMi=arrPack&255; int price=0; for(int k=si;k<sj;k++) price+=t.price[k];
    if(seatLeft>=num){ for(int k=si;k<sj;k++) t.seat[dayIdx*t.segN + k]-=num; // record success order
        if(users[ui].ocnt==users[ui].ocap){ int newCap = users[ui].ocap? users[ui].ocap*2: 4; Order *nw=new Order[newCap]; for(int i=0;i<users[ui].ocnt;i++) nw[i]=users[ui].orders[i]; delete[] users[ui].orders; users[ui].orders=nw; users[ui].ocap=newCap; }
        Order o; o.status="success"; o.trainID=tid; o.from=from; o.to=to; o.depDay=depDay; o.depH=depH; o.depMi=depMi; o.arrDay=arrDay; o.arrH=arrH; o.arrMi=arrMi; o.price=price; o.num=num; users[ui].orders[users[ui].ocnt++]=o; printf("%d\n", price*num); return 0;
    } else {
        if(qflag){ if(users[ui].ocnt==users[ui].ocap){ int newCap = users[ui].ocap? users[ui].ocap*2: 4; Order *nw=new Order[newCap]; for(int i=0;i<users[ui].ocnt;i++) nw[i]=users[ui].orders[i]; delete[] users[ui].orders; users[ui].orders=nw; users[ui].ocap=newCap; }
            Order o; o.status="pending"; o.trainID=tid; o.from=from; o.to=to; o.depDay=depDay; o.depH=depH; o.depMi=depMi; o.arrDay=arrDay; o.arrH=arrH; o.arrMi=arrMi; o.price=price; o.num=num; users[ui].orders[users[ui].ocnt++]=o; printf("queue\n"); return 0; }
        else { return -1; }
    }
}

int query_order(const string &uname){ int ui=userIndex.get(uname); if(ui==-1||!users[ui].online) return -1; int n=users[ui].ocnt; printf("%d\n", n); for(int i=n-1;i>=0;i--){ Order &o=users[ui].orders[i]; int dm,dd, am,ad; absToDate(o.depDay,dm,dd); absToDate(o.arrDay,am,ad); string depStr=fmtDate(dm,dd)+" "+fmtTime(o.depH,o.depMi); string arrStr=fmtDate(am,ad)+" "+fmtTime(o.arrH,o.arrMi); printf("[%s] %s %s -> %s %s %d %d\n", o.status.c_str(), o.trainID.c_str(), o.from.c_str(), o.to.c_str(), arrStr.c_str(), o.price, o.num); }
    return 0; }

int refund_ticket(const string &uname,int nth){ int ui=userIndex.get(uname); if(ui==-1||!users[ui].online) return -1; int idx=users[ui].ocnt - nth; if(idx<0 || idx>=users[ui].ocnt) return -1; Order &o=users[ui].orders[idx]; if(o.status=="refunded") return -1; int ti=trainIndex.get(o.trainID); if(ti==-1) return -1; Train &t=trains[ti]; int si=findStation(t,o.from); int sj=findStation(t,o.to); int offsetMin=0; for(int k=0;k<si;k++){ offsetMin+=t.travel[k]; if(k<=t.stationNum-2-1) offsetMin+=t.stop[k]; } int offsetDay=offsetMin/1440; int baseStartAbs=o.depDay - offsetDay; int dayIdx=baseStartAbs - t.saleStartAbs; if(o.status=="success"){ for(int k=si;k<sj;k++) t.seat[dayIdx*t.segN + k]+=o.num; } o.status="refunded"; printf("0\n"); return 0; }

int main(){ ios::sync_with_stdio(false); cin.tie(nullptr);
    users = new User[USER_CAP]; trains = new Train[TRAIN_CAP];
    string line; while(true){ if(!std::getline(cin,line)) break; if(line.empty()) continue; string *tok; int cnt; splitTokens(line,tok,cnt); if(cnt==0){ delete[] tok; continue; } string cmd=tok[0];
        if(cmd=="add_user"){ string c,u,p,n,m,gs; getArg(tok,cnt,"-c",c); getArg(tok,cnt,"-u",u); getArg(tok,cnt,"-p",p); getArg(tok,cnt,"-n",n); getArg(tok,cnt,"-m",m); getArg(tok,cnt,"-g",gs); int g=gs.empty()?0:stoi(gs); printf("%d\n", add_user(c,u,p,n,m,g)); }
        else if(cmd=="login"){ string u,p; getArg(tok,cnt,"-u",u); getArg(tok,cnt,"-p",p); printf("%d\n", login(u,p)); }
        else if(cmd=="logout"){ string u; getArg(tok,cnt,"-u",u); printf("%d\n", logout(u)); }
        else if(cmd=="query_profile"){ string c,u; getArg(tok,cnt,"-c",c); getArg(tok,cnt,"-u",u); string out; int r=query_profile(c,u,out); if(r==-1) printf("-1\n"); else printf("%s\n", out.c_str()); }
        else if(cmd=="modify_profile"){ string c,u,p,n,m,gs; bool hp=getArg(tok,cnt,"-p",p); bool hn=getArg(tok,cnt,"-n",n); bool hm=getArg(tok,cnt,"-m",m); bool hg=getArg(tok,cnt,"-g",gs); getArg(tok,cnt,"-c",c); getArg(tok,cnt,"-u",u); const string *pp= hp? &p: nullptr; const string *np= hn? &n: nullptr; const string *mp= hm? &m: nullptr; int gval=0; const int *gp= nullptr; if(hg){ gval=stoi(gs); gp=&gval; } string out; int r=modify_profile(c,u,pp,np,mp,gp,out); if(r==-1) printf("-1\n"); else printf("%s\n", out.c_str()); }
        else if(cmd=="add_train"){ string id, ns, seats, sstr, pstr, startT, tstr, ostr, dstr, ty; getArg(tok,cnt,"-i",id); getArg(tok,cnt,"-n",ns); getArg(tok,cnt,"-m",seats); getArg(tok,cnt,"-s",sstr); getArg(tok,cnt,"-p",pstr); getArg(tok,cnt,"-x",startT); getArg(tok,cnt,"-t",tstr); getArg(tok,cnt,"-o",ostr); getArg(tok,cnt,"-d",dstr); getArg(tok,cnt,"-y",ty); int n=stoi(ns); int seat=stoi(seats); int res=add_train(id,n,seat,sstr,pstr,startT,tstr,ostr,dstr,ty[0]); printf("%d\n",res); }
        else if(cmd=="release_train"){ string id; getArg(tok,cnt,"-i",id); printf("%d\n", release_train(id)); }
        else if(cmd=="query_train"){ string date,id; getArg(tok,cnt,"-d",date); getArg(tok,cnt,"-i",id); int r=query_train(id,date); if(r==-1) printf("-1\n"); }
        else if(cmd=="delete_train"){ string id; getArg(tok,cnt,"-i",id); printf("%d\n", delete_train(id)); }
        else if(cmd=="query_ticket"){ string s,t,d,pref; getArg(tok,cnt,"-s",s); getArg(tok,cnt,"-t",t); getArg(tok,cnt,"-d",d); bool hasp=getArg(tok,cnt,"-p",pref); if(!hasp) pref="time"; query_ticket(s,t,d,pref); }
        else if(cmd=="buy_ticket"){ string u,i,d,ns,f,t,q; getArg(tok,cnt,"-u",u); getArg(tok,cnt,"-i",i); getArg(tok,cnt,"-d",d); getArg(tok,cnt,"-n",ns); getArg(tok,cnt,"-f",f); getArg(tok,cnt,"-t",t); bool qf=false; if(getArg(tok,cnt,"-q",q)) qf=(q=="true"); int n=stoi(ns); int r=buy_ticket(u,i,d,n,f,t,qf); if(r==-1) printf("-1\n"); }
        else if(cmd=="query_order"){ string u; getArg(tok,cnt,"-u",u); int r=query_order(u); if(r==-1) printf("-1\n"); }
        else if(cmd=="refund_ticket"){ string u, ns; getArg(tok,cnt,"-u",u); int n=1; if(getArg(tok,cnt,"-n",ns)) n=stoi(ns); int r=refund_ticket(u,n); if(r==-1) printf("-1\n"); }
        else if(cmd=="clean"){ userIndex=StrIntMap(1<<17); trainIndex=StrIntMap(1<<15); userCount=0; trainCount=0; printf("0\n"); }
        else if(cmd=="exit"){ printf("bye\n"); delete[] tok; break; }
        else { printf("-1\n"); }
        delete[] tok;
    }
    return 0;
}
