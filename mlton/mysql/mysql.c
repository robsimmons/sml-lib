
#include <mysql.h>

#define mm_FIELD_TYPE_TINY         1
#define mm_FIELD_TYPE_SHORT        2
#define mm_FIELD_TYPE_LONG         3
#define mm_FIELD_TYPE_INT24        4
#define mm_FIELD_TYPE_LONGLONG     5
#define mm_FIELD_TYPE_DECIMAL      6
#define mm_FIELD_TYPE_FLOAT        7
#define mm_FIELD_TYPE_DOUBLE       8
#define mm_FIELD_TYPE_TIMESTAMP    9
#define mm_FIELD_TYPE_DATE         10
#define mm_FIELD_TYPE_TIME         11
#define mm_FIELD_TYPE_DATETIME     12
#define mm_FIELD_TYPE_YEAR         13
#define mm_FIELD_TYPE_STRING       14
#define mm_FIELD_TYPE_VAR_STRING   15
#define mm_FIELD_TYPE_BLOB         16
#define mm_FIELD_TYPE_SET          17
#define mm_FIELD_TYPE_ENUM         18
#define mm_FIELD_TYPE_NULL         19
#define mm_FIELD_TYPE_CHAR         20

/* not easy to subscript in mlton if we have a pointer 
   rather than array */
int mlton_mysql_get_length_i(unsigned long * l, int i) {
  return (int)l[i];
}

int mlton_mysql_is_null_i(MYSQL_ROW r, int i) {
  return !r[i];
}

int mlton_mysql_get_fieldtype_i(char * r_c, int i) {
  MYSQL_RES * r = (MYSQL_RES *)r_c;
  MYSQL_FIELD * mf = mysql_fetch_field_direct(r, i);
  switch(mf->type) {
  case FIELD_TYPE_TINY: return mm_FIELD_TYPE_TINY;
  case FIELD_TYPE_SHORT: return mm_FIELD_TYPE_SHORT;
  case FIELD_TYPE_LONG: return mm_FIELD_TYPE_LONG;
  case FIELD_TYPE_INT24: return mm_FIELD_TYPE_INT24;
  case FIELD_TYPE_LONGLONG: return mm_FIELD_TYPE_LONGLONG;
  case FIELD_TYPE_DECIMAL: return mm_FIELD_TYPE_DECIMAL;
  case FIELD_TYPE_FLOAT: return mm_FIELD_TYPE_FLOAT;
  case FIELD_TYPE_DOUBLE: return mm_FIELD_TYPE_DOUBLE;
  case FIELD_TYPE_TIMESTAMP: return mm_FIELD_TYPE_TIMESTAMP;
  case FIELD_TYPE_DATE: return mm_FIELD_TYPE_DATE;
  case FIELD_TYPE_TIME: return mm_FIELD_TYPE_TIME;
  case FIELD_TYPE_DATETIME: return mm_FIELD_TYPE_DATETIME;
  case FIELD_TYPE_YEAR: return mm_FIELD_TYPE_YEAR;
  case FIELD_TYPE_STRING: return mm_FIELD_TYPE_STRING;
  case FIELD_TYPE_VAR_STRING: return mm_FIELD_TYPE_VAR_STRING;
  case FIELD_TYPE_BLOB: return mm_FIELD_TYPE_BLOB;
  case FIELD_TYPE_SET: return mm_FIELD_TYPE_SET;
  case FIELD_TYPE_ENUM: return mm_FIELD_TYPE_ENUM;
  case FIELD_TYPE_NULL: return mm_FIELD_TYPE_NULL;
    // case FIELD_TYPE_CHAR: return mm_FIELD_TYPE_CHAR;
  default: return 0; /* error */
  }
}

/* must check that the row entry is not null first */
void mlton_mysql_get_data_i (char * r_c, int idx, char * dest, int len) {
  MYSQL_ROW * r = (MYSQL_ROW*) r_c;
  memcpy(dest, r[idx], len);
}
