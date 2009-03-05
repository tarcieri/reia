# 
# ODBC: Reia ODBC interface
# Copyright (C)2008 Jared Kuolt
#
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

# This interface is subject to change!
class ODBC
  # Open a ODBC connection with DSN string
  #
  # Example:
  #   db = ODBC("DSN=mysqldb;UID=root") 
  #   db.select_count("SELECT * FROM fruit") # Returns number of rows
  #
  # You can access results from the cursor by using select_count
  # and the first, last, next and prev methods. Otherwise, the query
  # method may be simplest, though it loads all results into memory.
  #
  # Cursor:
  #   result = db.next() # Returns ODBCResult object
  #
  # All results:
  #   result = db.sql_query("SELECT * FROM fruit") # ODBCResult object
  # 
  # Using the hashes method on a ODBCResult object gives hash access
  # to each row returned.
  #
  #   result.hashes()[0]['color'] # Returns the "color" column of the 
  #                               # first row returned
  #
  # You can also access the size of the result:
  #   result.size() # Row count
  #
  def initialize(dsn)
    odbc::start()
    case odbc::connect(dsn.to_list(), [])
    when (~ok, ref)
      @ref = ref
    when (~error, reason)
      throw reason
    end
  end

  def select_count(q)
    case odbc::select_count(@ref, q.to_list())
    when (~ok, nrows)
      nrows
    when (~error, reason)
      throw reason
    end
  end

  def query(q)
    case odbc::sql_query(@ref, q.to_list())
    when (~selected, colnames, rows)
      ODBCResult(colnames, rows) 
    when (~updated, nrows)
      nrows
    when (~error, reason)
      throw reason
    end
  end

  def next
    case odbc::next(@ref)
    when (~selected, colnames, rows)
      ODBCResult(colnames, rows)
    when (~error, reason)
      throw reason
    end
  end

  def prev
    case odbc::prev(@ref)
    when (~selected, colnames, rows)
      ODBCResult(colnames, rows)
    when (~error, reason)
      throw reason
    end
  end

  def first
    case odbc::first(@ref)
    when (~selected, colnames, rows)
      ODBCResult(colnames, rows)
    when (~error, reason)
      throw reason
    end
  end
  
  def last
    case odbc::last(@ref)
    when (~selected, colnames, rows)
      ODBCResult(colnames, rows)
    when (~error, reason)
      throw reason
    end
  end

  def describe_table(table)
    case odbc::describe_table(@ref, table.to_list())
    when (~ok, description)
      description
    when (~error, reason)
      throw reason
    end
  end
  
  def close
    odbc::disconnect(@ref)
    odbc::stop()
  end
end

class ODBCResult
  def initialize(colnames, rows)
    @columns = colnames.map{|m| m.to_string() }
    @rowdata = rows.map do |row|
      row.to_list().map do |c| 
        if erlang::is_list(c)
          c.to_string()
        else
          c
        end
      end
    end
  end

  def hashes
    if @hashes
      @hashes
    else
      range = (0..(@columns.size() - 1))
      @hashes = @rowdata.map {|row| range.map{|i| (@columns[i], row[i])}.to_hash() }
    end
  end

  def rowdata
    @rowdata
  end

  def columns
    @columns
  end

  def size
    @rowdata.size()
  end
end