if true; then true; fi
if true; then true; elif false; then false; fi
if true; then true; else false; fi
if true; then true; elif false; then false; else false; fi
case x in x) true;; esac
select x; do true; done
select x in y z; do true; done
