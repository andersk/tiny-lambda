data T=T{a::T->T,(%)::ShowS}
i d=T(i. \x v->'(':d v++' ':x%v++")")d
l f v="(λ "++v++". "++f(i(\_->v))%('x':v)++")"
(?)=q.head.lex
q(v,s)k|v/="("=k(foldl1 a.lookup v)s|'λ':u<-s,[(w,_:t)]<-lex u=t? \b->k(\e->T<*>l$b.(:e).(,)w).tail|0<1=s? \f->(? \x->k(a.f<*>x).tail)
main=interact(? \f->(f[]%"x"++))