data T=T{a::T->T,(%)::ShowS}
i d=T(i. \x v->'(':d v++' ':x%v++")")d
l f v="(λ "++v++". "++f(i(\_->v))%('x':v)++")"
(?)=q.head.lex
q("(",'λ':s)k|[(w,_:t)]<-lex s=t? \b(_:u)->k(\e->T<*>l$b.(:e).(,)w)u
q("(",s)k=s? \f->(? \x(_:u)->k(a.f<*>x)u)
q(v,s)k=k(foldl1 a.lookup v)s
main=interact(? \f->(f[]%"x"++))