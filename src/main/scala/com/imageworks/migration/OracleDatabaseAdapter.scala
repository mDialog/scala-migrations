/*
 * Copyright (c) 2010 Sony Pictures Imageworks Inc.
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the
 * distribution.  Neither the name of Sony Pictures Imageworks nor the
 * names of its contributors may be used to endorse or promote
 * products derived from this software without specific prior written
 * permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE
 * COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package com.imageworks.migration

/**
 * Map the BIGINT SQL type to a NUMBER(19, 0).
 *
 * A few other databases, such as Derby, MySQL and PostgreSQL, treat
 * BIGINT as a 8-byte signed integer type.  On Oracle a NUMBER(19, 0)
 * is large enough to store any integers from -9223372036854775808 to
 * 9223372036854775807 but not any integers with more digits.  A
 * NUMBER(19, 0) does allow a larger range of values than the other
 * databases, from -9999999999999999999 to 9999999999999999999, but
 * this seems like an acceptable solution without using a CHECK
 * constraint.
 *
 * This behavior is different than Oracle's default.  If a column is
 * defined using "INTEGER" and not a "NUMBER", Oracle uses a
 * NUMBER(38) to store it:
 *
 * http://download-west.oracle.com/docs/cd/B19306_01/server.102/b14200/sql_elements001.htm#sthref218
 *
 * Using a NUMBER(19, 0) helps ensure the compatibility of any code
 * running against an Oracle database so that is does not assume it
 * can use 38-digit integer values in case the data needs to be
 * exported to another database or if the code needs to work with
 * other databases.  Columns wishing to use a NUMBER(38) should use a
 * DecimalType column.
 */
class OracleBigintColumnDefinition
  extends ColumnDefinition
  with ColumnSupportsDefault
  with ColumnSupportsAutoincrement
{
  override
  val sql = "NUMBER(19, 0)"
}

class OracleCharColumnDefinition(use_nchar_type: Boolean)
  extends ColumnDefinition
  with ColumnSupportsDefault
  with ColumnSupportsLimit
{
  override
  def sql = if (use_nchar_type)
              sqlForColumnType("NCHAR")
            else
              sqlForColumnType("CHAR")
}

class OracleDecimalColumnDefinition
  extends AbstractDecimalColumnDefinition
{
  override
  val decimalSqlName = "NUMBER"
}

class OracleBooleanColumnDefinition
  extends ColumnDefinition
{
  override 
  val sql = "NUMBER(1)"
}

/**
 * Map the INTEGER SQL type to a NUMBER(10, 0).
 *
 * A few other databases, such as Derby, MySQL and PostgreSQL, treat
 * INTEGER as a 4-byte signed integer type.  On Oracle a NUMBER(10, 0)
 * is large enough to store any integers from -2147483648 to
 * 2147483647 but not any integers with more digits.  A NUMBER(10, 0)
 * does allow a larger range of values than the other databases, from
 * -9999999999 to 9999999999, but this seems like an acceptable
 * solution without using a CHECK constraint.
 *
 * This behavior is different than Oracle's default.  If a column is
 * defined using "INTEGER" and not a "NUMBER", Oracle uses a
 * NUMBER(38) to store it:
 *
 * http://download-west.oracle.com/docs/cd/B19306_01/server.102/b14200/sql_elements001.htm#sthref218
 *
 * Using a NUMBER(10, 0) helps ensure the compatibility of any code
 * running against an Oracle database so that is does not assume it
 * can use 38-digit integer values in case the data needs to be
 * exported to another database or if the code needs to work with
 * other databases.  Columns wishing to use a NUMBER(38) should use a
 * DecimalType column.
 */
class OracleIntegerColumnDefinition
  extends ColumnDefinition
  with ColumnSupportsDefault
{
  override
  val sql = "NUMBER(10, 0)"
}

/**
 * Map the SMALLINT SQL type to a NUMBER(5, 0).
 *
 * A few other databases, such as Derby, MySQL and PostgreSQL, treat
 * SMALLINT as a 2-byte signed integer type.  On Oracle a NUMBER(5, 0)
 * is large enough to store any integers from -32768 to 32767 but not
 * any integers with more digits.  A NUMBER(5, 0) does allow a larger
 * range of values than the other databases, from -99999 to 99999, but
 * this seems like an acceptable solution without using a CHECK
 * constraint.
 *
 * This behavior is different than Oracle's default.  If a column is
 * defined using "INTEGER" and not a "NUMBER", Oracle uses a
 * NUMBER(38) to store it:
 *
 * http://download-west.oracle.com/docs/cd/B19306_01/server.102/b14200/sql_elements001.htm#sthref218
 *
 * Using a NUMBER(5, 0) helps ensure the compatibility of any code
 * running against an Oracle database so that is does not assume it
 * can use 38-digit integer values in case the data needs to be
 * exported to another database or if the code needs to work with
 * other databases.  Columns wishing to use a NUMBER(38) should use a
 * DecimalType column.
 */
class OracleSmallintColumnDefinition
  extends ColumnDefinition
  with ColumnSupportsDefault
{
  override
  val sql = "NUMBER(5, 0)"
}

class OracleVarbinaryColumnDefinition
  extends ColumnDefinition
  with ColumnSupportsDefault
  with ColumnSupportsLimit
{
  override
  def sql =
  {
    if (! limit.isDefined) {
      val message = "In Oracle, a RAW column must always specify its size."
      throw new IllegalArgumentException(message)
    }

    sqlForColumnType("RAW")
  }
}

class OracleVarcharColumnDefinition(use_nchar_type: Boolean)
  extends ColumnDefinition
  with ColumnSupportsDefault
  with ColumnSupportsLimit
{
  override
  def sql = if (use_nchar_type)
              sqlForColumnType("NVARCHAR2")
            else
              sqlForColumnType("VARCHAR2")
}

class OracleDatabaseAdapter(override val schemaNameOpt: Option[String])
  extends DatabaseAdapter(schemaNameOpt)
{
  override
  val unquotedNameConverter = UppercaseUnquotedNameConverter

  override
  val addingForeignKeyConstraintCreatesIndex = false

  override
  def columnDefinitionFactory
    (column_type: SqlType,
     character_set_opt: Option[CharacterSet]): ColumnDefinition =
  {
    val use_nchar_type =
      character_set_opt match {
        case None => {
          false
        }
        case Some(CharacterSet(Unicode)) => {
          true
        }
        case Some(set @ CharacterSet(name)) => {
          logger.warn("Ignoring '{}' as Oracle only supports specifying no " +
                      "explicit character set encoding, which defaults the " +
                      "column to use the database's character set, or " +
                      "Unicode.",
                      set)
          false
        }
      }

    column_type match {
      case BigintType =>
        new OracleBigintColumnDefinition
      case BlobType =>
        new DefaultBlobColumnDefinition
      case BooleanType =>
	new OracleBooleanColumnDefinition
      case CharType =>
        new OracleCharColumnDefinition(use_nchar_type)
      case DecimalType =>
        new OracleDecimalColumnDefinition
      case DoubleType =>
	new DefaultDoubleColumnDefinition
      case IntegerType =>
        new OracleIntegerColumnDefinition
      case SmallintType =>
        new OracleSmallintColumnDefinition
      case TextType =>
	new DefaultTextColumnDefinition
      case TimestampType =>
        new DefaultTimestampColumnDefinition
      case VarbinaryType =>
        new OracleVarbinaryColumnDefinition
      case VarcharType =>
        new OracleVarcharColumnDefinition(use_nchar_type)
    }
  }

  override protected
  def alterColumnSql(schema_name_opt: Option[String],
                     column_definition: ColumnDefinition): String =
  {
    val post = column_definition.postSql

    val sb = new java.lang.StringBuilder(512)
      .append("ALTER TABLE ")
      .append(quoteTableName(schema_name_opt, column_definition.getTableName))
      .append(" MODIFY (")
      .append(quoteColumnName(column_definition.getColumnName))
      .append(' ')
      .append(column_definition.toSql)
      .append(')')

    /* because of autoincrement, there is a second possible sql statement to
     * pack onto the end here */    
    if (post.isDefined) {
      sb.append(';')
      sb.append(post.get)
    }
    
    sb.toString
  }

  override
  def removeColumnSql(schema_name_opt: Option[String],
                      table_name: String,
                      column_name: String): String =
  {
    // Oracle requires COLUMN keyword.
    new java.lang.StringBuilder(512)
      .append("ALTER TABLE ")
      .append(quoteTableName(schema_name_opt, table_name))
      .append(" DROP COLUMN ")
      .append(quoteColumnName(column_name))
      .toString
  }

  override
  def grantSql(schema_name_opt: Option[String],
               table_name: String,
               grantees: Array[String],
               privileges: GrantPrivilegeType*): String =
  {
    // Check that no columns are defined for any SELECT privs
    for {
      SelectPrivilege(columns) <- privileges
      if !columns.isEmpty
    } {
      val message = "Oracle does not support granting select to " +
                    "individual columns"
      throw new IllegalArgumentException(message)
    }

    super.grantSql(schema_name_opt, table_name, grantees, privileges: _*)
  }

  override
  def revokeSql(schema_name_opt: Option[String],
                table_name: String,
                grantees: Array[String],
                privileges: GrantPrivilegeType*): String =
  {
    // Check that no columns are defined for any privs with columns
    for {
      PrivilegeWithColumns(columns) <- privileges
      if !columns.isEmpty
    } {
      val message = "Oracle does not support revoking permissions from " +
                    "individual columns"
      throw new IllegalArgumentException(message)
    }

    super.revokeSql(schema_name_opt, table_name, grantees, privileges: _*)
  }

  /**
   * Return the SQL text for the ON DELETE clause for a foreign key
   * relationship.
   *
   * Oracle rejects adding a foreign key relationship containing the
   * "ON DELETE RESTRICT" text, so do not generate any SQL text for
   * it.  The behavior is the same though.  Let any other unsupported
   * options pass through, such as "ON DELETE NO ACTION", in case
   * Oracle ever does support that clause, which it does not in 10g.
   *
   * @param on_delete_opt an Option[OnDelete]
   * @param the SQL text to append to the SQL to create a foreign key
   *        relationship
   */
  override
  def onDeleteSql(on_delete_opt: Option[OnDelete]): String =
  {
    on_delete_opt match {
      case Some(OnDelete(Restrict)) => ""
      case opt => super.onDeleteSql(opt)
    }
  }

  /* TODO - make this stuff work */

  override
  def postAutoincrementFromSequenceSql(table_name: String,
				       column_name: String,
				       sequence_name: String): Option[String] =
  {
    val trigger_name = table_name + "_" + sequence_name + "_trigger"

    Some(new java.lang.StringBuilder(512)
      .append("CREATE OR REPLACE TRIGGER ")
      .append(quoteTableName(trigger_name))
      .append(" BEFORE INSERT ON ")
      .append(quoteTableName(table_name))
      .append(" FOR EACH row BEGIN IF inserting THEN IF :NEW.")
      .append(quoteColumnName(column_name))
      .append(" IS NULL THEN SELECT ")
      .append(quoteSequenceName(sequence_name))
      .append(".nextval INTO :NEW.")
      .append(quoteColumnName(column_name))
      .append(" FROM dual;")
      .append(" END IF; END IF; END;")
      .append(" ALTER TRIGGER ")
      .append(quoteTableName(trigger_name))
      .append(" ENABLE;")
      .toString)
  }
}
