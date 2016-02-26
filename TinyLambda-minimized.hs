data T=T{a::T->T,(%)::ShowS}
i d=T(i. \x v->'(':d v++' ':x%v++")")d
l f v="(λ "++v++". "++f(i(\_->v))%('x':v)++")"
p=q.head.lex
q("(",'λ':s)|[(w,'.':t)]<-lex s,(b,')':u)<-p t=(\e->T<*>l$b.(:e).(,)w,u)
q("(",s)|(f,t)<-p s,(x,')':u)<-p t=(a.f<*>x,u)
q(v,s)=(foldl1 a.lookup v,s)
main=interact$(\(f,[n])->f[]%"x"++[n]).p