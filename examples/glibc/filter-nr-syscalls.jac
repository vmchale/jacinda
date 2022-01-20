fn drop(n, str) :=
  let val l := #str
    in substr str n l end;

{`0 !~ /^#define __NR_(unused|reserved)[0-9]+ /
  & `0 !~ /^#define __NR_(syscalls|arch_specific_syscall|(OABI_)?SYSCALL_BASE) /
  & `0 !~ /^#define __NR_(|64_|[NO]32_)Linux(_syscalls)? /
  & `0 ~ /^#define __NR_/}{drop 5 `2}
