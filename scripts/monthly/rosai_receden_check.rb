#!/usr/bin/ruby
# coding : utf-8
Encoding.default_external = "euc-jp" unless RUBY_VERSION == "1.8.7"

begin
  require "monthly/rosai_receden"
  require "monthly/rosai_receden_common"
  require "monthly/rosai_receden_error"
  require "date"
  require "nkf"
  require "jcode"
  require "json"
rescue LoadError => err
    STDERR.puts "#{err.class}\n#{err.message}\n#{err.backtrace.join("\n")}"
    open(@errlog,"w"){|f|
      f.print "#{err.class}\n#{err.message}\n#{err.backtrace.join("\n")}"
    }
    raise "#{err.message}\n#{err.backtrace.join("\n")}"
end


$KCODE = "UTF-8"


class DBMAIN

   def initialize
     @data  = nil
     @mcpIn = {"db" => {}}
     @mcpOut = {}
     @rc = MCP_OK
   end

   def exec(_func, _table=nil ,_pathname= nil, _query=nil)

     @mcpIn["func"] = _func
     @mcpIn["db"]["table"]=_table
     @mcpIn["db"]["pathname"]=_pathname
     @rc,@mcpOut,@data = monfunc(@mcpIn.to_json,_query.to_json)
     @mcpOut=JSON.parse(@mcpOut) unless @mcpOut.nil?
     @data=JSON.parse(@data) unless @data.nil?
   rescue
     @rc = -99
   ensure
     return @rc
   end

   def rc
     @rc
   end

   def data
     @data
   end

   def [](name)
     @data[name]
   end

end


class Csvline

  def set_tbl_tensu(tbl_tensu)
    @tbl_tensu=Hash.new
    @tbl_tensu=tbl_tensu
  end

  def tbl_tensu
    @tbl_tensu
  end

end


class KsnSort

  attr_accessor :khn_exists,:tsusoku_exists,:chuksncd,:chuksntsuban

  def initialize
    @khn_exists = false
    @tsusoku_exists = false
    @chuksncd = 0
    @chuksntsuban = 0
  end

  def clear
    initialize
  end

end

class Receden_check < Receden_common

  def initialize
    @cache_tensu = Hash.new
    @cache_byomei = Hash.new
    @cache_labor_sio = Hash.new
  end

  def select_tensu(hospnum,srycd,sryymd)
    ret=Hash.new
    sryymd=wtos(sryymd)
    if @cache_tensu.key?(srycd)
      @cache_tensu[srycd].each{|c|
        if sryymd >= c["YUKOSTYMD"] && sryymd <= c["YUKOEDYMD"]
          ret = c
          break
        end
      }
    else
      @cache_tensu[srycd] = Array.new
    end

    if ret.empty?
      db = DBMAIN.new
      query = {"HOSPNUM"   => hospnum,
               "SRYCD"     => srycd,
               "YUKOSTYMD" => sryymd,
               "YUKOEDYMD" => sryymd,
              }
      db.exec("DBSELECT","tbl_tensu","key",query)
      if db.rc == 0
        db.exec("DBFETCH","tbl_tensu","key")
        if db.rc == 0
          ret = db.data
          @cache_tensu[srycd] << db.data
        end
      end
      db.exec("DBCLOSECURSOR","tbl_tensu","key")
    end
    return ret

  end

  def select_byomei(byomeicd)
    ret=Hash.new
    if @cache_byomei.key?(byomeicd)
      ret = @cache_byomei[byomeicd]
    else
      db = DBMAIN.new
      query = {"BYOMEICD"   => byomeicd,
              }
      db.exec("DBSELECT","tbl_byomei","key",query)
      if db.rc == 0
        db.exec("DBFETCH","tbl_byomei","key")
        if db.rc == 0
          ret = db.data
          @cache_byomei[byomeicd] = db.data
        end
      end
      db.exec("DBCLOSECURSOR","tbl_byomei","key")
    end

    return ret

  end

  def select_labor_sio(syocd,sryymd)
    ret=Hash.new
    sryymd=wtos(sryymd)
    if @cache_labor_sio.key?(syocd)
      @cache_labor_sio[syocd].each{|c|
        if sryymd >= c["YUKOSTYMD"] && sryymd <= c["YUKOEDYMD"]
          ret = c
          break
        end
      }
    else
      @cache_labor_sio[syocd] = Array.new
    end

    if ret.empty?
      db = DBMAIN.new
      query = {"SYOCD"     => syocd,
               "YUKOSTYMD" => sryymd,
               "YUKOEDYMD" => sryymd
              }
      db.exec("DBSELECT","tbl_labor_sio","key",query)
      if db.rc == 0
        db.exec("DBFETCH","tbl_labor_sio","key")
        if db.rc == 0
          ret = db.data
          @cache_labor_sio[syocd] << db.data
        end
      end
      db.exec("DBCLOSECURSOR","tbl_labor_sio","key")
    end

    return ret

  end

  def edit_msg(csv,key)

    ret=""

    if csv.nil?
    else
      if csv.hash.key?("RECID")
        id=csv["RECID"].value
        if @info.rec.key?(id)
          if @info[id].hash.key?(key)
            myvalue=csv[key].value
            if myvalue.split(//).size > 20
               myvalue=myvalue.split(//)[0,4].join << "…"
            end
            ret=sprintf("%s［%s］",@info[id].hash[key].label,myvalue)
          end
        end
      end
    end

    return ret

  end

  def check_format(rece,rows)

    if rows.hash.key?("RECID")

      id=rows["RECID"].value

      if @info.rec.key?(id)
        @info[id].rows.each{|item|
          if item.required? && rows[item.name].value.strip == ""
              @errors.push("20210",rece,rows,item.name)
          else
            sjisStr=NKF.nkf("-Wsx", rows[item.name].value)
            if item.maxsize < sjisStr.bytesize
              @errors.push("25390",rece,rows,item.name)
            else
              if item.fixed? && item.maxsize > sjisStr.bytesize
                @errors.push("25530",rece,rows,item.name)
              end
            end

            err=false
            case item.type
            when NUMERIC
              if rows[item.name].value =~ /^(|[0-9]+)$/
              else
                err=true
              end
            when ALPHANUMERIC
              if rows[item.name].hankaku != rows[item.name].value
                err=true
              end
            when KANJI
              if rows[item.name].zenkaku == rows[item.name].value
                if rows[item.name].value =~ /〓/
                  @errors.push("25441",rece,rows,item.name)
                end
              else
                err=true
              end
            when KATAKANA
              if rows[item.name].katakana != rows[item.name].value
                if rows[item.name].value =~ /〓/
                  @errors.push("25441",rece,rows,item.name)
                else
                  err=true
                end
              end
            when TEXT
              if ( rows[item.name].zenkaku == rows[item.name].value ) ||
                ( rows[item.name].hankaku == rows[item.name].value )
                if rows[item.name].value =~ /〓/
                  @errors.push("25441",rece,rows,item.name)
                end
              else
                err=true
              end
            end
            if  err == true
              @errors.push("25430",rece,rows,item.name)
            end
          end
        }
      end
      if @info[id].rows.size != rows.rows.size
        @errors.push("25420",rece,rows)
      end
    end
  end

  def check_lv1(r)

    case r.ir.size
    when 0
      @errors.push("19281")
    when 1
      if r.receipt.empty?
        @errors.push("19283")
      else
        if (r.receipt[0].re.empty? ) || (r.ir[0].line != r.receipt[0].re[0].line - 1)
          @errors.push("19230")
        end
      end
      if r.ir[0]["SKYYM"].wtos == ""
        @errors.push("19284",nil,r.ir[0],"SKYYM")
      end
    else
      @errors.push("19282")
    end

    case r.rs.size
    when 0
      @errors.push("10001")
    when 1
    else
      @errors.push("10002")
    end
  end

  def check_rs(r)

    rs=r.rs.first

    if rs["TANKA"].value == ""
        @errors.push("10010",nil,rs,"TANKA")
    else
      case rs["TANKA"].value.to_i
      when 1150,1200
      else
        @errors.push("10020",nil,rs,"TANKA")
      end
    end

    if rs["HOSPKBN"].value == ""
        @errors.push("10050",nil,rs,"HOSPKBN")
    else
      case rs["HOSPKBN"].value
      when "1","2","3"
      else
        @errors.push("10060",nil,rs,"HOSPKBN")
      end
    end

    if rs["SKYYMD"].wYmdtos == ""
        @errors.push("10090",nil,rs,"SKYYMD")
    end

    if rs["ZIPCD"].value =~ /^(|[0-9]{7})$/
    else
        @errors.push("10130",nil,rs,"ZIPCD")
    end

    if rs["LOCATION"].value.strip == ""
        @errors.push("10140",nil,rs,"LOCATION")
    end

    if rs["NAME"].value.strip == ""
        @errors.push("10150",nil,rs,"NAME")
    end

    if rs["PREFCD"].value.strip == ""
      if rs["OFFICECD"].value.strip != ""
        @errors.push("10170",nil,rs,"PREFCD")
      end
    else
      if PREFCD.values.index(rs["PREFCD"].value)
        if rs["OFFICECD"].value.strip != ""
           tbl_labor_sio=select_labor_sio("#{rs["PREFCD"].value}1#{rs["OFFICECD"].value}",rs["SKYYMD"].value)
           if tbl_labor_sio.empty?
             @errors.push("10190",nil,rs,"OFFICECD")
           end
        end
      else
        @errors.push("10180",nil,rs,"PREFCD")
      end
    end

    if rs["HOSPCD"].value.strip == ""
      @errors.push("10200",nil,rs,"HOSPCD")
    else
        if rs["HOSPCD"].value =~ /^[0-9]*$/
        else
          @errors.push("10210",nil,rs,"HOSPCD")
        end
    end

    err_ryoedymd = false
    gokei = 0
    maisu = 0
    kyufu_key=Array.new

    r.receipt.each{|rece|
      #    RE,RRレコードは配列としているが、レセプト毎に通常は１件
      re=rece.re.first
      if rece.rr.empty?
      else
        rr=rece.rr.first
        if rs["SKYYMD"].wYmdtos != "" && rr["RYOEDYMD"].wYmdtos != ""
          if rs["SKYYMD"].wYmdtos < rr["RYOEDYMD"].wYmdtos
            err_ryoedymd = true
          end
        end
        gokei += rr["GOKEI"].value.to_i
        kyufu_key << rece.kyufu_key
      end
    }

    r.receipt.each{|rece|
      if kyufu_key.index(rece.kyufu_key) != kyufu_key.rindex(rece.kyufu_key)
        @errors.push("37650",rece)
      end
    }

    if err_ryoedymd == true
      @errors.push("10100",nil,rs,"SKYYMD")
    end

    if gokei != rs["SKYMONEY"].value.to_i
      @errors.push("10310",nil,rs,"SKYMONEY",nil,"労災レセプト合計額［#{gokei}］")
    end

    if r.receipt.size != rs["MAISU"].value.to_i
      @errors.push("10320",nil,rs,"MAISU",nil,"労災レセプト件数［#{ r.receipt.size}］")
    end

  end

  def check_re(r,rece)

    re=rece.re.first
    rr=rece.rr.first

    if rece.sbt.nyugai == NYUIN
      if re["NYUINYMD"].value.strip == ""
        @errors.push("20030",rece,re,"NYUINYMD")
      else
        if re["NYUINYMD"].wYmdtos == ""
          @errors.push("30210",rece,re,"NYUINYMD")
        else
          if rr["RYOEDYMD"].wYmdtos != "" && rr["RYOEDYMD"].wYmdtos < re["NYUINYMD"].wYmdtos
            @errors.push("30220",rece,re,"NYUINYMD",nil,"療養期間－末日[#{rr["RYOEDYMD"].value}")
          end
          if re["BIRTHDAY"].wYmdtos != "" && re["NYUINYMD"].wYmdtos < re["BIRTHDAY"].wYmdtos
            @errors.push("30230",rece,re,"NYUINYMD",nil,"生年月日[#{re["BIRTHDAY"].value}")
          end
        end
      end
    end

    if re["SRYKA1_CD"].value != ""
      if RECEKA.key?(re["SRYKA1_CD"].value)
      else
        @errors.push("20220",rece,re,"SRYKA1_CD")
      end
    end

    if re["SRYKA2_CD"].value != ""
      if RECEKA.key?(re["SRYKA2_CD"].value)
      else
        @errors.push("20220",rece,re,"SRYKA2_CD")
      end
    end

    if re["SRYKA3_CD"].value != ""
      if RECEKA.key?(re["SRYKA3_CD"].value)
      else
        @errors.push("20220",rece,re,"SRYKA3_CD")
      end
    end

    if re["BTUKBN"].value =~ /^(|(  |[0-9]{2})+)$/
      if rece.sbt.nyugai == NYUIN
        arry_btunum = re["BTUKBN"].value.scan(/.{2}/)
        
        arry_btunum.each{|btunum|
          case btunum.strip
          when "01","02","07",""
          else
            @errors.push("31190",rece,re,"BTUKBN")
            break
          end
        }
        if arry_btunum.size != arry_btunum.uniq.size
          @errors.push("41190",rece,re,"BTUKBN")
        end
      end
    else
      @errors.push("21660",rece,re,"BTUKBN")
    end

    if @skykbn == "2"
      if re["SELNUM"].value.strip == ""
        @errors.push("26510",rece,re,"SELNUM")
      end
    end

    if re["NAME"].value.strip == ""
      @errors.push("30010",rece,re,"NAME")
    end

    case re["SEX"].value
    when "1","2"
    else
      @errors.push("31330",rece,re,"SEX")
    end

    if re["BIRTHDAY"].wYmdtos == ""
      @errors.push("31450",rece,re,"BIRTHDAY")
    end

  end

  def check_rr(r,rece)

    re=rece.re.first
    rr=rece.rr.first

    if rr["JNISSU"].value.to_i != 999
      if rr["RYOSTYMD"].wtos != "" && rr["RYOSTYMD"].wtos <= H250531
        @errors.push("10410",rece,rr,"RYOSTYMD")
      end

      if rr["RYOSTYMD"].wYmdtos != "" &&  rr["RYOEDYMD"].wYmdtos != ""
         if rr["RYOSTYMD"].wYmdtos[0,6] != rr["RYOEDYMD"].wYmdtos[0,6]
           @errors.push("21630",rece,rr,"RYOSTYMD",nil,"療養期間－末日［#{ rr["RYOEDYMD"].value}］")
         end
         st=rr["RYOSTYMD"].wYmdtos
         ed=rr["RYOEDYMD"].wYmdtos
         nissu=Date.new(ed[0,4].to_i,ed[4,2].to_i,ed[6,2].to_i) - Date.new(st[0,4].to_i,st[4,2].to_i,st[6,2].to_i) + 1
         if nissu < rr["JNISSU"].value.to_i
           @errors.push("32010",rece,rr,"JNISSU",nil,"当該療養期間の日数[#{nissu}]")
         end
      end
    end

    if rr["TENKIKBN"].value.strip == ""
        @errors.push("20020",rece,rr,"TENKIKBN")
    else
      case rr["TENKIKBN"].value
      when "1","3","5","7","9"
      else
        @errors.push("38200",rece,rr,"TENKIKBN")
      end
    end

    if rr["SHINKEIKBN"].value.strip == ""
      @errors.push("20040",rece,rr,"SHINKEIKBN")
    else
      case rr["SHINKEIKBN"].value
      when "1","3","5","7"
      else
        @errors.push("38210",rece,rr,"SHINKEIKBN")
      end
    end

    if rece.sbt.form == CHOKI
      if rr["SHINKEIKBN"].value == "3"
        @errors.push("20050",rece,rr,"SHINKEIKBN")
      end
      if rr["SHOBYOYMD"].value.strip != ""
        @errors.push("20060",rece,rr,"SHOBYOYMD")
      end
      if rr["TANKINUM"].value.strip != ""
        @errors.push("38230",rece,rr,"TANKINUM")
      end
      if rr["CHOKINUM"].value.strip == ""
        @errors.push("38250",rece,rr,"CHOKINUM")
      end
    end

    if rece.sbt.form == TANKI
      if rr["SHOBYOYMD"].value.strip == ""
        @errors.push("20070",rece,rr,"SHOBYOYMD")
      else
        if rr["SHOBYOYMD"].wYmdtos == ""
          @errors.push("38040",rece,rr,"SHOBYOYMD")
        else
          if rr["RYOEDYMD"].wYmdtos != "" && rr["RYOEDYMD"].wYmdtos < rr["SHOBYOYMD"].wYmdtos
            @errors.push("38050",rece,rr,"SHOBYOYMD")
          end
          if re["BIRTHDAY"].wYmdtos != "" && re["BIRTHDAY"].wYmdtos > rr["SHOBYOYMD"].wYmdtos
            @errors.push("38060",rece,rr,"SHOBYOYMD")
          end
          if rr["RYOSTYMD"].wYmdtos != "" && rr["RYOSTYMD"].wYmdtos < rr["SHOBYOYMD"].wYmdtos
            @errors.push("38110",rece,rr,"SHOBYOYMD")
          end
        end
      end
      if rr["TANKINUM"].value.strip == ""
        @errors.push("38240",rece,rr,"TANKINUM")
      end
      if rr["CHOKINUM"].value.strip != ""
        @errors.push("38260",rece,rr,"CHOKINUM")
      end
      if rr["INFO"].value.strip == ""
        @errors.push("40100",rece,rr,"INFO")
      end
      if rr["LOCATION"].value.strip == ""
        @errors.push("40110",rece,rr,"LOCATION")
      end
    end

    if rr["KANANAME"].value.strip == ""
      @errors.push("20100",rece,rr,"KANANAME")
    end

    if rr["RYOSTYMD"].value.strip == ""
      @errors.push("20120",rece,rr,"RYOSTYMD")
    else
      if rr["RYOSTYMD"].wYmdtos == ""
        @errors.push("21600",rece,rr,"RYOSTYMD")
      else
        if re["BIRTHDAY"].wYmdtos != "" && re["BIRTHDAY"].wYmdtos > rr["RYOSTYMD"].wYmdtos
          @errors.push("38100",rece,rr,"RYOSTYMD")
        end
      end
    end

    if rr["RYOEDYMD"].value.strip == ""
      @errors.push("20130",rece,rr,"RYOEDYMD")
    else
      if rr["RYOEDYMD"].wYmdtos == ""
        @errors.push("21610",rece,rr,"RYOEDYMD")
      else
        if re["BIRTHDAY"].wYmdtos != "" && re["BIRTHDAY"].wYmdtos > rr["RYOEDYMD"].wYmdtos
          @errors.push("38160",rece,rr,"RYOEDYMD")
        end
        if rr["RYOSTYMD"].wYmdtos != "" && rr["RYOSTYMD"].wYmdtos > rr["RYOEDYMD"].wYmdtos
          @errors.push("38120",rece,rr,"RYOEDYMD")
        end
      end
    end

    if rr["RYOEDYMD"].wYmdtos[0,6] > r.ir.first["SKYYM"].wtos[0,6]
      @errors.push("21590",rece,rr,"RYOEDYMD")
    end

    if rr["KEIKA"].value.strip == ""
      @errors.push("20140",rece,rr,"KEIKA")
    end

    if rr["TENSU"].value.strip == ""
      @errors.push("20150",rece,rr,"TENSU")
    end

    if rr["SAIGAIKBN"].value.strip == ""
      @errors.push("20170",rece,rr,"SAIGAIKBN")
    else
      case rr["SAIGAIKBN"].value
      when "1","3"
      else
        @errors.push("20180",rece,rr,"SAIGAIKBN")
      end
    end

    if rr["FORMSBT"].value.strip == ""
      @errors.push("20190",rece,rr,"FORMSBT")
    else
      case rr["FORMSBT"].value
      when "2","3","4","5"
      else
        @errors.push("20200",rece,rr,"FORMSBT")
      end
    end

    if rece.sbt.nyugai == NYUIN
      if rr["SKJKAISU"].value.strip == ""
          @errors.push("38280",rece,rr,"SKJKAISU")
      else
         sryymd=wtos(rece.sryym)
         nissu =  Date.new(sryymd[0,4].to_i,sryymd[4,2].to_i,-1) - Date.new(sryymd[0,4].to_i,sryymd[4,2].to_i,1) + 1
         if rr["SKJKAISU"].value.to_i > nissu * 3
            @errors.push("35310",rece,rr,"SKJKAISU")
         end
      end
      if rr["SKJKINGAKU"].value.strip == ""
          @errors.push("38300",rece,rr,"SKJKINGAKU")
      else
         if rr["SKJKAISU"].value.to_i > rr["JNISSU"].value.to_i * 3
            @errors.push("35340",rece,rr,"SKJKAISU")
         end
      end
    end

    if rece.sbt.nyugai == GAIRAI
      if rr["SKJKAISU"].value.strip != ""
          @errors.push("38270",rece,rr,"SKJKAISU")
      end
      if rr["SKJKINGAKU"].value.strip != ""
          @errors.push("38290",rece,rr,"SKJKINGAKU")
      end
    end

  end

  def check_sy_tekiyo(r,rece)

    ir=r.ir.first
    re=rece.re.first
    rr=rece.rr.first
    rs=r.rs.first

    shoshin_OK_days=Array.new.fill(false,1..31)
    latest_sryymd="00000000"

    shokei_tensu = 0
    shokei_kingaku = 0
    shokuji_kaisu = 0
    shokuji_kingaku = 0

    if rece.sy.size >= 100
      @errors.push("22020",rece)
    end

    if rece.sy.empty?
      @errors.push("25540",rece)
    else
      flg_sryymd_error = true
      rece.sy.each{|sy|
        if sy["SRYYMD"].wYmdtos != ""
          if sy["SRYYMD"].wYmdtos > latest_sryymd
            latest_sryymd = sy["SRYYMD"].wYmdtos
          end
          if rr["RYOSTYMD"].wYmdtos != ""
            if rr["RYOSTYMD"].wYmdtos >= sy["SRYYMD"].wYmdtos
              flg_sryymd_error = false
            end
            if rr["RYOSTYMD"].wYmdtos[0,6]  == sy["SRYYMD"].wYmdtos[0,6] &&
               sy["SRYYMD"].wYmdtos <= rr["RYOEDYMD"].wYmdtos
              shoshin_OK_days[sy["SRYYMD"].wYmdtos[6,2].to_i] = true
            end
          end
          if re["BIRTHDAY"].wYmdtos != ""
            if re["BIRTHDAY"].wYmdtos > sy["SRYYMD"].wYmdtos
              @errors.push("32210",rece,sy,"SRYYMD")
            end
          end
        else
          @errors.push("32070",rece,sy,"SRYYMD")
        end
  
        if sy["BYOMEICD"].value == "0000999"
          if sy["BYOMEI"].value.gsub(/　/,"").strip == ""
            @errors.push("32090",rece,sy,"BYOMEI")
          end
        else
          tbl_byomei=select_byomei(sy["BYOMEICD"].value)
          if tbl_byomei.empty?
            @errors.push("32040",rece,sy,"BYOMEICD")
          end
        end
        if sy["SYUBYO"].value =~ /^(|01)$/
        else
          @errors.push("32200",rece,sy,"SYUBYO")
        end

        if sy["MODCD"].value.bytesize.modulo(4) != 0
          @errors.push("21680",rece,sy,"MODCD")
        else
          if  sy["MODCD"].value =~ /^(|(    |[0-9]{4})+)$/
            sy["MODCD"].value.scan(/[0-9]{4}/).each{|modcd|
              tbl_byomei=select_byomei(sprintf("ZZZ%s",modcd))
              if tbl_byomei.empty?
                @errors.push("32050",rece,sy,"MODCD",nil,modcd)
              end
            }
          else
            @errors.push("21680",rece,sy,"MODCD")
          end
        end
        #---------------------------------------------------------------------------------------------------
        #          フォーマットチェック(SYレコード)
        check_format(rece,sy)
        #---------------------------------------------------------------------------------------------------
      }
      if flg_sryymd_error == true
        @errors.push("32060",rece,rece.sy.first,nil,nil,"傷病名レコード１件目　診療開始日［#{rece.sy.first["SRYYMD"].value}］")
      end
    end


    if rece.tekiyo.empty?
      @errors.push("25550",rece,nil)
    end

    srtSrykbn="00"
    chozai_kaisu=0

    rece.tekiyo.each{|tekiyo|
      myzaikaisu=nil
      myreceline=0
      flg_plus  = false
      flg_minus = false
      flg_kingaku_santei = false
      flg_shokuji = false
      ksnsort = KsnSort.new

      tekiyo.zai.each_with_index{|zai,zai_idx|
        case zai["RECID"].value
        when "RI","IY","TO","CO"
          zai.set_tbl_tensu(select_tensu(@hospnum,zai["SRYCD"].value,rece.sryym))
          if zai.tbl_tensu.empty?
          else
            if tekiyo.srykbn == "97" &&
               zai["RECID"].value == "RI" &&
               zai.tbl_tensu["NYUTENTTLKBN"].to_i == 970 &&
               zai.tbl_tensu["KOKUJISKBKBN2"].to_i == 1
               flg_shokuji = true
            end
          end
        else
          zai.set_tbl_tensu(nil)
        end
      }

      tekiyo.zai.each_with_index{|zai,zai_idx|

        tbl_tensu = zai.tbl_tensu

        if rece.sbt.exists?
        else
          next
        end

        if zai.hash.key?("SRYKBN") && zai["SRYKBN"].value != ""

          flg_plus  = false
          flg_minus = false

          if zai["SRYKBN"].value < srtSrykbn
            @errors.push("23060",rece,zai,"SRYKBN",nil,"前回診療識別［#{srtSrykbn}］")
          end

          srtSrykbn = zai["SRYKBN"].value

        end

        case zai["RECID"].value
        when  "RI","IY","TO","CO"
          tbl_tensu=select_tensu(@hospnum,zai["SRYCD"].value,rece.sryym)
          if tbl_tensu["TENSIKIBETU"].to_i == 7
            flg_minus = true
          else
            flg_plus  = true
          end

          if zai["SRYCD"].hankaku[0,1] =~ /[1678]/
            if tbl_tensu.empty?
              @errors.push("33031",rece,zai,"SRYCD")
            else
              case  zai["RECID"].value
              when  "RI"

                case tbl_tensu["BYOSYOKBN"].to_i
                when 1,2,6
                  if re["BEDSU"].value.to_i < 1
                    @errors.push("36390",rece,re,"BEDSU",nil,tbl_tensu["NAME"])
                  end
                end

                if tbl_tensu["KZMCOMPSIKIBETU"].to_i == 1
                  if zai["SURYO"].value.to_i == 0
                    @errors.push("33040",rece,zai,"SURYO",nil,tbl_tensu["NAME"])
                  else
                    case tbl_tensu["KZMERR"].to_i
                    when 2,3
                      if  tbl_tensu["KZMKGN"].to_i - tbl_tensu["KZM"].to_i >= zai["SURYO"].value.to_i
                        myfooter=sprintf("%s %d%s",tbl_tensu["NAME"],zai["SURYO"].value.to_i,tbl_tensu["TANINAME"])
                        @errors.push("33170",rece,zai,"SURYO",nil,myfooter)
                      end
                    end
                  end
                end

                if tekiyo.srykbn == "97"
                  if tbl_tensu["NYUTENTTLKBN"].to_i >= 970 and tbl_tensu["NYUTENTTLKBN"].to_i <= 975
                  else
                    myfooter=sprintf("診療識別［%s］ %s",tekiyo.srykbn,tbl_tensu["NAME"])
                    @errors.push("34910",rece,zai,nil,nil,myfooter)
                  end

                  if flg_shokuji == true
                    if tbl_tensu["NYUTENTTLKBN"].to_i == 970 and tbl_tensu["KOKUJISKBKBN2"].to_i == 1
                      shokuji_kaisu   += zai["SURYO"].value.to_i   * zai["KAISU"].value.to_i
                    end
                    shokuji_kingaku += zai["KINGAKU"].value.to_i * zai["KAISU"].value.to_i
                  end

                else
                  if tbl_tensu["NYUTENTTLKBN"].to_i >= 970 and tbl_tensu["NYUTENTTLKBN"].to_i <= 975
                    myfooter=sprintf("診療識別［%s］ %s",tekiyo.srykbn,tbl_tensu["NAME"])
                    @errors.push("34900",rece,zai,nil,nil,myfooter)
                  end

                  shokei_tensu   += zai["TEN"].value.to_i   * zai["KAISU"].value.to_i
                  shokei_kingaku += zai["KINGAKU"].value.to_i * zai["KAISU"].value.to_i

                end
                case tbl_tensu["KOKUJISKBKBN1"].to_i
                when 1,3,5
                  ksnsort.clear
                  ksnsort.khn_exists = true
                  ksnsort.chuksncd = tbl_tensu["CHUKSNCD"].to_i
                when 7
                  if ksnsort.tsusoku_exists == true
                    @errors.push("44360",rece,zai,nil,nil,tbl_tensu["NAME"])
                  else
                    if tbl_tensu["CHUKSNCD"].to_i != 0
                      case
                      when ksnsort.khn_exists == false
                        @errors.push("46410",rece,zai,"CHUKSNCD",nil,tbl_tensu["NAME"])
                      when tbl_tensu["CHUKSNCD"].to_i != ksnsort.chuksncd
                        @errors.push("46411",rece,zai,"CHUKSNCD",nil,tbl_tensu["NAME"])
                      when tbl_tensu["CHUKSNTSUBAN"].to_i == ksnsort.chuksntsuban
                        @errors.push("46120",rece,zai,"CHUKSNTSUBAN",nil,tbl_tensu["NAME"])
                      when tbl_tensu["CHUKSNTSUBAN"].to_i < ksnsort.chuksntsuban
                        @errors.push("46410",rece,zai,"CHUKSNCD",nil,tbl_tensu["NAME"])
                      else
                        ksnsort.chuksntsuban = tbl_tensu["CHUKSNTSUBAN"].to_i
                      end
                    end
                  end
                when 9
                  ksnsort.tsusoku_exists = true
                  if ksnsort.khn_exists == false
                    @errors.push("44350",rece,zai,nil,nil,tbl_tensu["NAME"])
                  end
                end
                if tbl_tensu["SRYKBN"] == "11" && tbl_tensu["KOKUJISKBKBN2"] == "1"
                  if rr["SHINKEIKBN"].value != "1"
                    @errors.push("40070",rece,rr,"SHINKEIKBN")
                  end
                end
                if tbl_tensu["NYUTENTTLKBN"].to_i == 240  && tbl_tensu["KOKUJISKBKBN2"] == "1"
                  chozai_kaisu += zai["KAISU"].value.to_i
                end
                if tbl_tensu["NYUTENTTLKBN"].to_i == 240  && tbl_tensu["KOKUJISKBKBN2"] == "1"
                  chozai_kaisu += zai["KAISU"].value.to_i
                end
                case rs["HOSPKBN"].value
                when "1","2"
                  if tbl_tensu["HOSPSRYKBN"] == "2"
                    @errors.push("44240",rece,zai,"SRYCD",nil,"#{tbl_tensu["NAME"]}")
                  end
                when "3"
                  if tbl_tensu["HOSPSRYKBN"] == "1"
                    @errors.push("46030",rece,zai,"SRYCD",nil,"#{tbl_tensu["NAME"]}")
                  end
                end
              when  "IY"

                case tbl_tensu["YKZKBN"].to_i
                when 8,9
                   @errors.push("23950",rece,zai,"SRYCD",nil,tbl_tensu["NAME"])
                end

                case tbl_tensu["TENSIKIBETU"].to_i
                when 1
                 if zai["SURYO"].value.to_f == 0
                   @errors.push("33090",rece,zai,"SURYO",nil,tbl_tensu["NAME"])
                 end
                end

                shokei_tensu   += zai["TEN"].value.to_i   * zai["KAISU"].value.to_i

              when  "TO"

                case tbl_tensu["TENSIKIBETU"].to_i
                when 1,2,4,9
                 if zai["SURYO"].value.to_f == 0
                   @errors.push("33110",rece,zai,"SURYO",nil,tbl_tensu["NAME"])
                 end
                end

                case tbl_tensu["TENSIKIBETU"].to_i
                when 1,2,4
                 case
                 when tbl_tensu["TANICD"].to_i == 0 && zai["TANICD"].value.to_i == 0
                   @errors.push("43230",rece,zai,"TANICD",nil,tbl_tensu["NAME"])
                 when zai["TANICD"].value.to_i < 1 ||  zai["TANICD"].value.to_i > 60
                   @errors.push("34650",rece,zai,"TANICD",nil,tbl_tensu["NAME"])
                 end
                   
                end

                case tbl_tensu["TENSIKIBETU"].to_i
                when 2
                  if zai["TANKA"].value.to_i == 0
                   @errors.push("33130",rece,zai,"TANKA",nil,tbl_tensu["NAME"])
                  end
                end

                if zai["SRYCD"].value == "777770000"
                  if zai["NAME"].value.gsub(/　/,"").strip == "" && zai["INFO"].value.gsub(/　/,"").strip == ""
                   @errors.push("33160",rece,zai,"NAME",nil,tbl_tensu["NAME"])
                   @errors.push("33160",rece,zai,"INFO",nil,tbl_tensu["NAME"])
                 end
                end

                shokei_tensu   += zai["TEN"].value.to_i   * zai["KAISU"].value.to_i

              when  "CO"
              end
            end
          else
            @errors.push("23040",rece,zai,"SRYCD")
          end
        end

        flg_kaisu_edit = false

        date_error = {"23130" => false ,"23170" => false ,"44550" => false,"44510" => false,
                      "46340" => false }

        zai.hash.each{|key,row|
          if key =~ /^COMCD/
            mycommoji=sprintf("%s%s","COMMENT",key.sub(/^COMCD/,""))
            if row.value  != ""
              com_tensu=select_tensu(@hospnum,row.value,rece.sryym)
              if row.value[0,1] != "8"
                @errors.push("23070",rece,zai,key)
              else
                myfooter=""
                if com_tensu.empty?
                  @errors.push("34500",rece,zai,key)
                else
                  myfooter=com_tensu["NAME"]
                end
                case row.value[1,2]
                when "10" , "30"
                  if zai[mycommoji].value.gsub(/　/,"").strip == ""
                    @errors.push("34410",rece,zai,mycommoji,nil,myfooter)
                  end
                when "40","41"
                  if zai[mycommoji].value.gsub(/　/,"").strip == ""
                    @errors.push("34410",rece,zai,mycommoji,nil,myfooter)
                  elsif zai[mycommoji].value.gsub(/^[０-９]+$/,"").strip == ""
                    if com_tensu.empty?
                    else
                      comsize= com_tensu["SSTKIJUNCD2"].to_i + com_tensu["SSTKIJUNCD4"].to_i  + com_tensu["SSTKIJUNCD6"].to_i + com_tensu["SSTKIJUNCD8"].to_i
                      if zai[mycommoji].value.split(//).size != comsize
                        @errors.push("34470",rece,zai,mycommoji,nil,myfooter)
                      end
                    end
                  else
                    @errors.push("34440",rece,zai,mycommoji,nil,myfooter)
                  end
                when "90"
                  if zai[mycommoji].value =~ /^(　{4}|[０-９]{4})+$/
                     modcds=zai[mycommoji].value.scan(/[０-９]{4}/)
                     case
                     when modcds.empty?
                       @errors.push("34410",rece,zai,mycommoji)
                     when modcds.size > 5
                       @errors.push("23870",rece,zai,mycommoji)
                     else
                       modcds.each{|modcd|
                         tbl_byomei=select_byomei(sprintf("ZZZ%s",modcd.tr("０-９","0-9")))
                         if tbl_byomei.empty?
                           @errors.push("34590",rece,zai,mycommoji,nil,modcd)
                         end
                       }
                     end
                  else
                    @errors.push("23860",rece,zai,mycommoji)
                  end
                end
              end
            else
              if zai[mycommoji].value != ""
                @errors.push("23880",rece,zai,key,nil,"コメント文字［#{zai[mycommoji].value}］")
              end
            end
          end
          if key =~ /^DAY[0-3][0-9]$/
            if row.value  =~ /^[0-9]+$/
              if row.value.to_i == 0
                date_error["23130"] = true
              end
              mysryymd = sprintf("%s%s",wtos(rece.sryym)[0,6],key.sub(/DAY/,""))

              day_idx = key.sub(/DAY/,"").to_i
              if mysryymd < rr["RYOSTYMD"].wYmdtos
                date_error["44550"] = true
              end
              if tbl_tensu.empty?
              else
                if tbl_tensu["JITUDAY"].to_i == 2 && tbl_tensu["DAYCNT"].to_i == 1
                  if latest_sryymd[0,6] < wtos(rece.sryym)[0,6]
                    date_error["44510"] = true
                  end
                  if rr["SHINKEIKBN"].value == "1" && rr["RYOSTYMD"].wYmdtos == mysryymd
                  else
                    date_error["46340"] = true
                  end
                end
              end
            end

            if row.value  != ""
              if wtos(sprintf("%s%s",rece.sryym,key.sub(/DAY/,""))) == ""
                date_error["23170"] = true
              end
              flg_kaisu_edit = true
            end
          end
        }
        date_error.each{|key,row|
          if row == true
            myfooter=tbl_tensu["NAME"] + "算定日："
            ("01".."31").each{|day|
              if  zai["DAY#{day}"].value.to_i > 0
                myfooter << "、#{day.to_i}日"
              end
            }
            myfooter.sub!(/：、/,"：")
            @errors.push(key,rece,zai,nil,nil,myfooter)
          end
        }
        case zai["RECID"].value
        when  "RI","IY","TO"
          if flg_kaisu_edit == false
            @errors.push("23190",rece,zai,nil,nil,tbl_tensu["NAME"])
          end
        end
        #---------------------------------------------------------------------------------------------------
        #                   フォーマットチェック(SI,IY,TO,COレコード)
        case  zai["RECID"].value
        when  "RI"
          check_format(rece,zai)
        when  "IY"
          check_format(rece,zai)
        when  "TO"
          check_format(rece,zai)
        when  "CO"
          check_format(rece,zai)
        end
        #---------------------------------------------------------------------------------------------------

        case zai["RECID"].value
        when  "RI","IY","TO"

          if zai["TEN"].value.strip != ""

            if zai["RECID"].value == "IY"
              if flg_minus == true
                if flg_plus == true
                  @errors.push("33190",rece,zai)
                else
                  if zai["TEN"].value.to_i == 0
                    @errors.push("33060",rece,zai,"TEN",nil,tbl_tensu["NAME"])
                  end
                end
              end
            end
          end

          if zai["KAISU"].value.to_i == 0
            @errors.push("23110",rece,zai,"KAISU")
          end

          if myzaikaisu.nil?
            myzaikaisu = zai["KAISU"].value
            myreceline = zai.receline
          else
            if  myzaikaisu != zai["KAISU"].value
              myfooter=sprintf("レセプト内レコード番号［%d］の回数［%s］",myreceline,myzaikaisu)
              @errors.push("23120",rece,zai,"KAISU",nil,myfooter)
            end
          end

          if zai["RECID"].value  == "RI" &&  tbl_tensu["TENSIKIBETU"].to_i == 1
            flg_kingaku_santei = true
          end
          flg_tensu_required = false
          case
          when ( tekiyo.zai[zai_idx + 1] == nil ) ||
               ( tekiyo.zai[zai_idx + 1].hash.key?("RECID") != nil  && zai["RECID"].value != tekiyo.zai[zai_idx + 1]["RECID"].value ) ||
               ( tekiyo.zai[zai_idx + 1].hash.key?("SRYKBN") == nil ) ||
               ( tekiyo.zai[zai_idx + 1]["SRYKBN"].value != "" )
            flg_tensu_required = true
          else
            if zai["RECID"].value == "RI"
              case
              when ( tbl_tensu["TENSIKIBETU"].to_i == 4 || tbl_tensu["TENSIKIBETU"].to_i == 7 )
                flg_tensu_required = true
              when ( tekiyo.srykbn =~ /^(11|12|13|14|97)$/) ||
                   ( tbl_tensu["JITUDAY"].to_i == 4 && tbl_tensu["DAYCNT"].to_i == 0 )
                case tbl_tensu["KOKUJISKBKBN1"].to_i
                when 1,3,5,7,9,8
                  if tekiyo.zai[zai_idx + 1].tbl_tensu.empty?
                    flg_tensu_required = true
                  else
                    if tekiyo.zai[zai_idx + 1].tbl_tensu["KOKUJISKBKBN1"].to_i == 7 ||
                       tekiyo.zai[zai_idx + 1].tbl_tensu["KOKUJISKBKBN1"].to_i == 9 ||
                       tekiyo.zai[zai_idx + 1].tbl_tensu["KOKUJISKBKBN1"].to_i == 8 
                    else
                      flg_tensu_required = true
                    end
                  end
                end
              end
            end
          end

          if flg_tensu_required == true
            if zai["RECID"].value == "RI"
              if @check_level == "1"
                if zai["TEN"].value.to_i == 0 && zai["KINGAKU"].value.to_i == 0
                  @errors.push("38350",rece,zai,nil,nil,tbl_tensu["NAME"])
                end
                if flg_kingaku_santei == true && zai["KINGAKU"].value.to_i == 0
                  @errors.push("38380",rece,zai,"KINGAKU",nil,tbl_tensu["NAME"])
                end
              end
              if flg_kingaku_santei == false && zai["KINGAKU"].value.to_i != 0
                @errors.push("38390",rece,zai,"KINGAKU",nil,tbl_tensu["NAME"])
              end
            else
              if ( @check_level == "1" )
                if zai["TEN"].value.to_i == 0
                  @errors.push("33070",rece,zai,"TEN",nil,tbl_tensu["NAME"])
                end
              end
            end
            flg_tensu_santei   = false
            flg_kingaku_santei = false
          end
        end

        if zai["RECID"].value == "CO"

          myfooter=""
          if tbl_tensu.empty?
          else
            myfooter=tbl_tensu["NAME"]
          end

          if zai["SRYCD"].value[0,1] != "8"
            @errors.push("23070",rece,zai,"SRYCD")
          else
            case zai["SRYCD"].value[1,2]
            when "10" , "30"
              if zai["DATA"].value.gsub(/　/,"").strip == ""
                @errors.push("34380",rece,zai,"DATA",nil,myfooter)
              end
            when "40","41"
              if zai["DATA"].value.gsub(/　/,"").strip == ""
                @errors.push("34380",rece,zai,"DATA",nil,myfooter)
              elsif zai["DATA"].value.gsub(/^[０-９]+$/,"").strip == ""
                if tbl_tensu.empty?
                else
                  comsize= tbl_tensu["SSTKIJUNCD2"].to_i + tbl_tensu["SSTKIJUNCD4"].to_i  + tbl_tensu["SSTKIJUNCD6"].to_i + tbl_tensu["SSTKIJUNCD8"].to_i
                  if zai["DATA"].value.split(//).size != comsize
                    @errors.push("34400",rece,zai,"DATA",nil,myfooter)
                  end
                end
              else
                @errors.push("34390",rece,zai,"DATA",nil,myfooter)
              end
            when "90"
              if zai["DATA"].value =~ /^(　{4}|[０-９]{4})+$/
                 modcds=zai["DATA"].value.scan(/[０-９]{4}/)
                 if modcds.empty?
                   @errors.push("34380",rece,zai,"DATA")
                 else
                   modcds.each{|modcd|
                     
                     tbl_byomei=select_byomei(sprintf("ZZZ%s",modcd.tr("０-９","0-9")))
                     if tbl_byomei.empty?
                       @errors.push("33930",rece,zai,"DATA",nil,modcd)
                     end
                   }
                 end
              else
                @errors.push("23800",rece,zai,"DATA")
              end
            end
          end
        end

        if tekiyo.srykbn =~ /^(01|99)$/
          if zai["RECID"].value == "CO"
          else
            @errors.push("23810",rece,zai,"RECID")
          end
        end
      }
    }

    if chozai_kaisu > rr["JNISSU"].value.to_i
      @errors.push("40090",rece,nil,nil,nil,"調剤料算定回数［#{chozai_kaisu}］　診療実日数［#{rr["JNISSU"].value}］")
    end

    if rr["TENSU"].value.to_i != shokei_tensu
      @errors.push("48410",rece,rr,"TENSU",nil,"適用データの点数合計［#{shokei_tensu}］")
    end

    case rs["TANKA"].value.to_i
    when 1150,1200
      if (rr["TENSU"].value.to_i * rs["TANKA"].value.to_i / 100).to_i != rr["TENSU_KANZAN"].value.to_i
        @errors.push("48420",rece,rr,"TENSU_KANZAN")
      end
    end

    if rr["KINGAKU"].value.to_i  != shokei_kingaku
      @errors.push("48430",rece,rr,"KINGAKU",nil,"適用データの金額合計［#{shokei_kingaku}］")
    end

    if rece.sbt.nyugai == NYUIN

      if rr["TENSU_KANZAN"].value.to_i +  rr["KINGAKU"].value.to_i + rr["SKJKINGAKU"].value.to_i  != rr["GOKEI"].value.to_i
        @errors.push("48440",rece,rr,"GOKEI")
      end

      if rr["SKJKINGAKU"].value.to_i  != shokuji_kingaku
        @errors.push("48480",rece,rr,"SKJKINGAKU",nil,"適用データの金額合計［#{shokuji_kingaku}］")
      end

      if rr["SKJKAISU"].value.to_i  != shokuji_kaisu
        @errors.push("48470",rece,rr,"SKJKAISU",nil,"適用データの合計回数［#{shokuji_kaisu}］")
      end
    end

    if rece.sbt.nyugai == GAIRAI
      if rr["TENSU_KANZAN"].value.to_i +  rr["KINGAKU"].value.to_i   != rr["GOKEI"].value.to_i
        @errors.push("48450",rece,rr,"GOKEI")
      end
    end
  end

  def check_sj(r,rece)

    re=rece.re.first

    if rece.sj.flatten.size >= 1000
      @errors.push("23820",rece)
    end
    rece.sj.each_with_index{|sj,i|
      if i == 0 && sj[0]["SJKBN"].value == ""
        @errors.push("23830",rece,sj[0],"SJKBN")
      else
        if SJKBN.key?(sj[0]["SJKBN"].hankaku)
        else
          @errors.push("23840",rece,sj[0],"SJKBN")
        end
      end
      sj.each{|sj_line|
        #---------------------------------------------------------------------------------------------------
        #              フォーマットチェック(SJレコード)
        check_format(rece,sj_line)
        #---------------------------------------------------------------------------------------------------
      }
    }
  end

  def check_receipt(r)

    ir=r.ir.first
    rs=r.rs.first
    receNum=1

    #---------------------------------------------------------------------------------------------------
    #    フォーマットチェック(IR,RSレコード)
    check_format(nil,ir)
    check_format(nil,rs)
    #---------------------------------------------------------------------------------------------------

    check_rs(r)

    r.receipt.each{|rece|

      #    REレコードは配列としているが、レセプト毎に通常は１件
      re=rece.re.first

      if wtos(rece.sryym) == ""
        @errors.push("21611",rece,re,"SKYINF")
        next
      end

      if rece.rr.empty?
        @errors.push("25400",rece,re)
        next
      end

      if rece.rr.size > 1
        @errors.push("25470",rece,re)
        next
      end

      #    RRレコードは配列としているが、レセプト毎に通常は１件
      rr=rece.rr.first

      #---------------------------------------------------------------------------------------------------
      #       フォーマットチェック(RE,RRレコード)
      check_format(rece,re)
      check_format(rece,rr)
      #---------------------------------------------------------------------------------------------------

      check_re(r,rece)
      check_rr(r,rece)
      check_sy_tekiyo(r,rece)
      check_sj(r,rece)

      if re["RECENUM"].num != nil
        if re["RECENUM"].value.bytesize > 6
          @errors.push("19100",rece,re,"RECENUM")
        else
          if  re["RECENUM"].value.to_i != receNum
            @errors.push("19170",rece,re,"RECENUM")
          end
          receNum = re["RECENUM"].value.to_i + 1
        end
      else
        @errors.push("19100",rece,re,"RECENUM")
        receNum += 1
      end


    }
  end

  def main(_param)

    @hospnum,@recefile,@infile,@outfile,@errlog,@skykbn,@check_level=_param.split(",")

    db = DBMAIN.new
    db.exec("DBOPEN")
    db.exec("DBSTART")
    @info=Recinfo.new
    @errors=Receden_error.new(@recefile)

    r=Receden.new(@infile)
    check_lv1(r)
    if @errors.empty?
      check_receipt(r)
    end

    @errors.write(@outfile)

    db.exec("DBCOMMIT")
    db.exec("DBDISCONNECT")

  rescue => err
    STDERR.puts "#{err.class}\n#{err.message}\n#{err.backtrace.join("\n")}"
    open(@errlog,"w"){|f|
      f.print "#{err.class}\n#{err.message}\n#{err.backtrace.join("\n")}"
    }
    raise "#{err.message}\n#{err.backtrace.join("\n")}"
  end

end
