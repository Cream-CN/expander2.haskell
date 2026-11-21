/* expander2.dll 的最小 C 客户端：链接导入库，展示全套调用约定。
 *
 * 构建（以正常版为例，dll 与 expander2.h 都在 .build-dll 里）：
 *
 *   clang -O1 -o example.exe expander2_example.c -I. -L. -lexpander2
 *   .\example.exe
 *
 * 兼容版换 -lexpander2-compat。运行时 DLL 必须能被找到（放同目录即可）。
 *
 * 约定回顾：
 *   - 先 hs_init，再调用任何 EXPANDER2_* / 记号入口；退出前 hs_exit。
 *   - 所有返回的 char* 都要 EXPANDER2_FREE，不能自己 free。
 *   - 返回 NULL = 失败，错误文本用 EXPANDER2_LAST_ERROR() 取。
 */

#include "expander2.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef _WIN32
#include <windows.h>
#endif

static int failures = 0;

static void check(int cond, const char *what) {
  if (!cond) {
    printf("FAIL: %s\n", what);
    failures++;
  }
}

/* 取一条结果并打印；失败时打印错误槽位。返回 1 表示成功。 */
static int show(const char *label, char *out) {
  if (out == NULL) {
    char *err = EXPANDER2_LAST_ERROR();
    printf("%-22s ERR  %s\n", label, err ? err : "(no error text)");
    EXPANDER2_FREE(err);
    return 0;
  }
  printf("%-22s %s\n", label, out);
  EXPANDER2_FREE(out);
  return 1;
}

int main(int argc, char **argv) {
#ifdef _WIN32
  SetConsoleOutputCP(65001); /* 结果里可能有中文/全角字符 */
#endif

  hs_init(&argc, &argv);

  char *build = EXPANDER2_BUILD();
  printf("dll build            %s\n", build ? build : "(null)");
  EXPANDER2_FREE(build);

  /* 1) 记号直调：序列型，term 作为第二参数 */
  show("PPS(1,2,3, term=3)", PPS("1,2,3", 3));
  show("PPS_ALTER(1,2,3)", PPS_ALTER("1,2,3", 3));

  /* 2) 文本型：BMS v1.0 */
  show("BMS_V1((0,0)(1,1))", BMS_V1("(0,0)(1,1)", 2));

  /* 3) 参数化记号：param 写在第三参数（0..25） */
  show("ABS_BM1YNSS((0)(1),p0)", ABS_BM1YNSS("(0)(1)", 1, 0));

  /* 4) 通用分派：mode 0=fs，1=alter */
  show("EXPAND(pps fs)", EXPANDER2_EXPAND("pps", EXPANDER2_MODE_FS, 3, 0, "1,2,3"));
  show("EXPAND(pps alter)", EXPANDER2_EXPAND("pps", EXPANDER2_MODE_ALTER, 3, 0, "1,2,3"));

  /* 5) 失败路径 1：序列型 term=0 非法
       失败路径 2：参数越界（bt1ynss 的 param 是 1..25） */
  check(PPS("1,2", 0) == NULL, "term=0 must fail");
  show("EXPAND(bt1ynss p0)", EXPANDER2_EXPAND("bt1ynss", EXPANDER2_MODE_FS, 1, 0, "(0)(1)"));

  /* 6) 注册表自省 */
  int n = EXPANDER2_COUNT();
  printf("notations            %d\n", n);
  check(n == 114, "registry must hold 114 notations");
  for (int i = 0; i < 3 && i < n; i++) {
    char *id = EXPANDER2_ID(i);
    char *name = EXPANDER2_NAME(i);
    char *sym = EXPANDER2_SYMBOL(i);
    int kind = EXPANDER2_KIND(i);
    int pmin = EXPANDER2_PARAM_MIN(i);
    int pmax = EXPANDER2_PARAM_MAX(i);
    int pdef = EXPANDER2_PARAM_DEFAULT(i);
    if (pmin >= 0) {
      printf("  [%d] id=%-14s sym=%-12s kind=%d param=%d..%d default=%d\n", i, id, sym, kind, pmin, pmax, pdef);
    } else {
      printf("  [%d] id=%-14s sym=%-12s kind=%d\n", i, id, sym, kind);
    }
    EXPANDER2_FREE(id);
    EXPANDER2_FREE(name);
    EXPANDER2_FREE(sym);
  }

  /* 7) 大结果：一次分配/释放循环，验证没有泄漏式契约错误 */
  for (int r = 0; r < 3; r++) {
    char *out = EXPANDER2_EXPAND("pps4", EXPANDER2_MODE_FS, 3, 0, "1,1,2,3");
    check(out != NULL, "pps4 must succeed");
    EXPANDER2_FREE(out);
  }

  hs_exit();

  printf(failures ? "\nRESULT: %d FAILURE(S)\n" : "\nRESULT: ok\n", failures);
  return failures ? 1 : 0;
}
