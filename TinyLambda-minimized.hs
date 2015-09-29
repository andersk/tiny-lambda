data T=T{a::T->T,(%)::ShowS}
i d=T(i. \x v->'(':d v++' ':x%v++")")d
l f v="(λ "++v++". "++f(i(\_->v))%('x':v)++")"
p s=readParen(0<1)q s++[(foldl1 a.lookup v,t)|(v,t)<-lex s]
q('λ':s)=[(\e->T<*>l$b.(:e).(,)w,u)|(w,'.':t)<-lex s,(b,u)<-p t]
q s=[(a.f<*>x,u)|(f,t)<-p s,(x,u)<-p t]
main=interact(\s->do(f,[n])<-p s;f[]%"x"++[n])