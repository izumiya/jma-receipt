#!/usr/bin/ruby

require "monthly/receden"
require "monthly/receden_common"
require "monthly/receden_error"
require	"daily/apslib"
require	"date"
require	"kconv"
require	"jcode"

class Csvline

  def set_tbl_tensu(tbl_tensu)
    @tbl_tensu=Hash.new
    @tbl_tensu=tbl_tensu
  end

  def set_ten_required(flg)
    @ten_required=flg
  end

  def tbl_tensu
    @tbl_tensu
  end

  def ten_required?
    @ten_required
  end
end


class KsnSort

  attr_accessor :khn_exists,:tsusoku_exists,:chuksncd,:chuksntsuban

  def initialize
    @khn_exists == false
    @tsusoku_exists == false
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
    @cache_hknjainf = Hash.new
    @cache_byomei = Hash.new
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
      tbl=PandaTable.new(@db,"tbl_tensu")
      tbl["tbl_tensu.HOSPNUM"]   = hospnum
      tbl["tbl_tensu.SRYCD"]     = srycd
      tbl["tbl_tensu.YUKOSTYMD"] = sryymd
      tbl["tbl_tensu.YUKOEDYMD"] = sryymd
      if tbl.execFunction("DBSELECT","key") == 0
        if tbl.execFunction("DBFETCH","key") == 0
          tbl.values.each{|key,value|
            ret[key.gsub(/^tbl_tensu\./,"")]=value
          }
          @cache_tensu[srycd] << ret
        end
      end
      tbl.execFunction("DBCLOSECURSOR","key")
    end
    return ret

  end

  def select_hknjainf(hospnum,hknjanum)
    ret=Hash.new
    if @cache_hknjainf.key?(hknjanum)
        ret = @cache_hknjainf[hknjanum]
    else
      tbl=PandaTable.new(@db,"tbl_hknjainf")
      tbl["tbl_hknjainf.HOSPNUM"]   = hospnum
      tbl["tbl_hknjainf.HKNJANUM"]     = hknjanum
      if tbl.execFunction("DBSELECT","key") == 0
        if tbl.execFunction("DBFETCH","key") == 0
          tbl.values.each{|key,value|
            ret[key.gsub(/^tbl_hknjainf\./,"")]=value
          }
          @cache_hknjainf[hknjanum] =  ret
        end
      end
      tbl.execFunction("DBCLOSECURSOR","key")
    end

    return ret

  end

  def select_byomei(byomeicd)
    ret=Hash.new
    if @cache_byomei.key?(byomeicd)
      ret = @cache_byomei[byomeicd]
    else
      tbl=PandaTable.new(@db,"tbl_byomei")
      tbl["tbl_byomei.BYOMEICD"] = byomeicd
      if tbl.execFunction("DBSELECT","key") == 0
        if tbl.execFunction("DBFETCH","key") == 0
          tbl.values.each{|key,value|
            ret[key.gsub(/^tbl_byomei\./,"")]=value
          }
          @cache_byomei[byomeicd] = ret
        end
      end
      tbl.execFunction("DBCLOSECURSOR","key")
    end

    return ret

  end

  def edit_msg(csv,key)

    ret=""

    if csv.nil?
    else
      if csv.hash.key?("RECID")
        id=csv["RECID"].value
        if id =~ /^(1|2|3|4|8|11|12|13|14)$/
          id="SAI"
        end
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
          if item.maxsize < rows[item.name].value.bytesize
            @errors.push("25390",rece,rows,item.name)
          else
            if item.fixed? && item.maxsize > rows[item.name].value.bytesize
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
        }
      end
      if @info[id].rows.size != rows.rows.size
        @errors.push("25420",rece,rows)
      end
    end
  end

  def check_lv1(r)

    ir=nil

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
  end

  def check_receipt(r)


    ir=r.ir.first
    receNum=1

    #---------------------------------------------------------------------------------------------------
    #    フォーマットチェック(IRレコード)
    check_format(nil,ir)
    #---------------------------------------------------------------------------------------------------

    r.receipt.each{|rece|

      #    REレコードは配列としているが、レセプト毎に通常は１件
      re=rece.re.first

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
      if rece.sbt.exists?
        if rece.sbt.sskkn == SYAKOK || rece.sbt.sskkn == ir["SSKKN"].hankaku
        else
          @errors.push("21500",rece,re,"RECESBT")
        end
      else
        @errors.push("21500",rece,re,"RECESBT")
      end

      if re["SRYYM"].wtos == ""
        @errors.push("21600",rece,re,"SRYYM")
      else
        if ir["SKYYM"].wtos != "" 
          if re["SRYYM"].wtos >= ir["SKYYM"].wtos
            @errors.push("21590",rece,re,"SRYYM")
          end
#         ３年（請求時効期間）経過チェック
          if re["SRYYM"].wtos.to_i + 30000 < ir["SKYYM"].wtos.to_i
            @errors.push("31030",rece,re,"SRYYM")
          end
        end

        if re["BIRTHDAY"].wtos != ""
          myApril="#{re["SRYYM"].wtos[0,4]}0401"
          if re["SRYYM"].wtos[4,2].to_i <= 3
            myApril=(myApril.to_i - 10000).to_s
		  end
          if rece.sbt.honkzk == ROKUSAI
            if re["BIRTHDAY"].wtos.to_i <= myApril.to_i - 60000
              @errors.push("31290",rece,re,"BIRTHDAY")
            end
          end
          if rece.sbt.honkzk != ROKUSAI && rece.sbt.honkzk != KOUHI
            if re["BIRTHDAY"].wtos.to_i >  myApril.to_i - 60000
              @errors.push("31300",rece,re,"BIRTHDAY")
            end
          end
          if re["BIRTHDAY"].wtos[0,6] > re["SRYYM"].wtos[0,6]
            @errors.push("31350",rece,re,"BIRTHDAY")
          end
          if ( rece.sbt.honkzk == HONNIN || rece.sbt.honkzk == KAZOKU )
            if rece.age >= 70
              @errors.push("31380",rece,re,"BIRTHDAY")
            end
          end
          if rece.sbt.honkzk == KOREI7 || rece.sbt.honkzk == KOREIIPN
            if rece.age < 70 || rece.age >= 75
              @errors.push("31430",rece,re,"BIRTHDAY")
            end
          end
          if ir["SSKKN"].value == SYAHO && rece.sbt.honkzk == HONNIN
            myYmd1=sprintf("%s00",re["SRYYM"].wtos[0,6]).to_i
            myYmd2=sprintf("%s00",re["BIRTHDAY"].wtos[0,6]).to_i
            if ((myYmd1 - myYmd2) / 10000).to_i < 15
              @errors.push("31400",rece,re,"BIRTHDAY")
            end
          end
        end

      end

      case re["SEX"].value
      when "1","2"
      else
        @errors.push("31330",rece,re,"SEX")
      end

      if re["BIRTHDAY"].wtos == ""
        @errors.push("31450",rece,re,"BIRTHDAY")
      end

      if re["TOKKI"].value =~ /^(|(  |[0-9]{2})+)$/

        if re["SRYYM"].wtos >= H210501
          if rece.tokki.include?("17")
            if [HONNIN,ROKUSAI,KAZOKU,KOREI7,KOUKI7].include?(rece.sbt.honkzk)
            else
               @errors.push("30110",rece,re,"TOKKI")
            end
          end

          if rece.tokki.include?("18") || rece.tokki.include?("19")
            if [HONNIN,ROKUSAI,KAZOKU,KOREIIPN,KOUKIIPN].include?(rece.sbt.honkzk)
            else
               @errors.push("30120",rece,re,"TOKKI")
            end
          end
        end

        if re["SRYYM"].wtos >= H210801
          if rece.tokki.include?("22")
            if [HONNIN,ROKUSAI,KAZOKU,KOREI7,KOUKI7].include?(rece.sbt.honkzk) &&
               rece.sbt.nyugai == NYUIN
            else
               @errors.push("30130",rece,re,"TOKKI")
            end
          end

          if rece.tokki.include?("23") || rece.tokki.include?("24")
            if [HONNIN,ROKUSAI,KAZOKU].include?(rece.sbt.honkzk) &&
               rece.sbt.nyugai == NYUIN
            else
               @errors.push("30140",rece,re,"TOKKI")
            end
          end
        end

        if re["SRYYM"].wtos >= H210501 &&
           re["SRYYM"].wtos <  H210801
          if rece.tokki.include?("22") || rece.tokki.include?("23") || rece.tokki.include?("24")
            @errors.push("30150",rece,re,"TOKKI")
          end
        end

        if ((rece.tokki.include?("17") || rece.tokki.include?("18") || rece.tokki.include?("19")) && [KOREI7,KOUKI7,KOREIIPN,KOUKIIPN].include?(rece.sbt.honkzk)) ||
           ( rece.tokki.include?("22") && [KOREI7,KOUKI7].include?(rece.sbt.honkzk)) ||
           ((rece.tokki.include?("22") || rece.tokki.include?("23") || rece.tokki.include?("24")) && [HONNIN,ROKUSAI,KAZOKU].include?(rece.sbt.honkzk))
          myflg=false
          rece.ko.each{|ko|
            if ko["FTNNUM"].value =~ /^(51..601|51..602|52)/
              myflg=true
              break
            end
          }
          if myflg == false
            @errors.push("30160",rece,re,"TOKKI")
          end
        end

        if re["SRYYM"].wtos >= H240301

          if rece.sbt.ho_count == 1 &&
            rece.sbt.ko_count >= 1 &&
            rece.sbt.nyugai == NYUIN &&
            rece.tokki.include?("96") &&
            !(rece.ho.empty?) &&
            (rece.ho[0]["DCKBN"].value == "2" || rece.ho[0]["DCKBN"].value == "3")

            rece.ko.each{|ko|
              if ko["TEN"].value != "0"
                myfooter=sprintf("公費法別[%s]点数[%s]",ko["FTNNUM"].value[0,2],ko["TEN"].value)
                @errors.push("30170",rece,re,"TOKKI",nil,myfooter)
              end
            }

          end

          if rece.sbt.nyugai == GAIRAI &&
            rece.tokki.include?("96") &&
            !(rece.ho.empty?) &&
            (rece.ho[0]["DCKBN"].value == "2" || rece.ho[0]["DCKBN"].value == "3")

            if rece.sbt.ko_count >= 1
                @errors.push("30180",rece,re,"TOKKI")
            end

          end
        end

        if rece.sbt.honkzk != KOREIIPN && rece.tokki.include?("20")
          @errors.push("31050",rece,re,"TOKKI")
        end

        if (rece.tokki.include?("02") && rece.tokki.include?("16")) ||
           (rece.tokki.include?("02") && rece.tokki.include?("03")) ||
           (rece.tokki.include?("16") && rece.tokki.include?("03"))
          @errors.push("31060",rece,re,"TOKKI")
        end

        if ir["SSKKN"].value == SYAHO &&
           (rece.sbt.honkzk == HONNIN || rece.sbt.honkzk == KOUHI ) &&
           (rece.tokki.include?("21"))
          @errors.push("31170",rece,re,"TOKKI")
        end

        if re["SRYYM"].wtos >= H210101 &&
           rece.sbt.honkzk == KOREI7 &&
           rece.tokki.include?("15")
          @errors.push("31180",rece,re,"TOKKI")
        end

        if (rece.tokki & ["17","18","19","22","23","24"]).size >= 2
          @errors.push("31280",rece,re,"TOKKI")
        end
      else
        @errors.push("21650",rece,re,"TOKKI")
      end

      if rece.sbt.nyugai == NYUIN
        if re["NYUINYMD"].wtos == ""
          @errors.push("30210",rece,re,"NYUINYMD")
        else
          if re["SRYYM"].wtos != "" && re["SRYYM"].wtos[0,6] < re["NYUINYMD"].wtos[0,6]
            @errors.push("30220",rece,re,"NYUINYMD")
          end

          if re["BIRTHDAY"].wtos != "" && re["BIRTHDAY"].wtos > re["NYUINYMD"].wtos
            @errors.push("30230",rece,re,"NYUINYMD")
          end
        end
      end

      if re["BTUKBN"].value =~ /^(|(  |[0-9]{2})+)$/
        if rece.sbt.nyugai == NYUIN
          re["BTUKBN"].value.scan(/.{2}/).each{|btunum|
            case btunum.strip
            when "01","02","07",""
            else
              @errors.push("31190",rece,re,"BTUKBN")
              break
            end
          }
        end
      else
        @errors.push("21660",rece,re,"BTUKBN")
      end

      if re["NAME"].value.gsub(/( |　)/,"").strip == ""
        @errors.push("30010",rece,re,"NAME")
      end

      if rece.sbt.honkzk == KOREI7 || rece.sbt.honkzk == KOUKI7
        if re["SKJFTNKBN"].value != ""
          @errors.push("35300",rece,re,"SKJFTNKBN")
        end
      end

      if rece.sbt.nyugai == NYUIN
        case rece.sbt.honkzk
        when KOREIIPN,KOUKIIPN
          case re["SKJFTNKBN"].value
          when "","1","2","3","4"
          else
            @errors.push("35370",rece,re,"SKJFTNKBN")
          end

          if rece.tokki.include?("19")
            if re["SKJFTNKBN"].value == ""
              @errors.push("35500",rece,re,"SKJFTNKBN")
            end
          end

        when HONNIN,ROKUSAI,KAZOKU
          case re["SKJFTNKBN"].value
          when "","1","2"
          else
            @errors.push("35370",rece,re,"SKJFTNKBN")
          end
        end
      end

      if rece.sbt.nyugai == GAIRAI
        case rece.sbt.honkzk
        when KOREIIPN,KOUKIIPN
          case re["SKJFTNKBN"].value
          when "","1","3","4"
          else
            @errors.push("35390",rece,re,"SKJFTNKBN")
          end
        when HONNIN,ROKUSAI,KAZOKU
          case re["SKJFTNKBN"].value
          when "","1"
          else
            @errors.push("35390",rece,re,"SKJFTNKBN")
          end
        end
      end

      #---------------------------------------------------------------------------------------------------
      #       フォーマットチェック(REレコード)
      check_format(rece,re)
      #---------------------------------------------------------------------------------------------------

      check_sikaku(ir,rece)
      check_sy_tekiyo(ir,rece)
      check_sj(ir,rece)
      check_sai(ir,rece)

    }
  end

  def check_sikaku(ir,rece)

    re=rece.re.first

    rece.ho.each{|ho|


      if ir["SSKKN"].value == SYAHO &&
        rece.sbt.ko_count == 0
        case rece.sbt.honkzk
        when HONNIN,KOREIIPN,KOREI7
          case ho["RIYU"].value
          when "1","2","3",""
          else
            @errors.push("31160",rece,ho,"RIYU")
          end
        end
      end

      if ho["JNISSU"].value == ""
        @errors.push("30310",rece,ho,"JNISSU")
      else
        if re["SRYYM"].nissu != nil && re["SRYYM"].nissu < ho["JNISSU"].value.to_i
          @errors.push("32010",rece,ho,"JNISSU")
        end
      end

      if ho["KIGO"].value == ""
      else
        case
        when ho["KIGO"].value =~ /( |　)/
          @errors.push("21630",rece,ho,"KIGO")
        else
          case ho["HKNJANUM"].value[0,2]
          when "01"
            if ( ho["KIGO"].value.split(//).size != 7 &&
                 ho["KIGO"].value.split(//).size != 8 ) ||
               ho["KIGO"].value.gsub(/０/,"") == "" ||
               ho["KIGO"].value.gsub(/[０-９]/,"") != ""
              @errors.push("31480",rece,ho,"KIGO")
            end

            if ho["NUM"].value.split(//).size >= 8 ||
               ho["NUM"].value.gsub(/０/,"") == "" ||
               ho["NUM"].value.gsub(/[０-９]/,"") != ""
              @errors.push("31500",rece,ho,"NUM")
            end
          when "02"
            if re["SRYYM"].wtos >= H230401
              if ho["KIGO"].value.split(//).size > 10 ||
                 ho["KIGO"].value.gsub(/０/,"") == ""  ||
                 ho["KIGO"].value.gsub(/[０-９]/,"") != ""
                @errors.push("31980",rece,ho,"KIGO")
              end

              if ho["NUM"].value.gsub(/０/,"") == ""  ||
                 ho["NUM"].value.gsub(/[０-９]/,"") != ""
                @errors.push("31990",rece,ho,"NUM")
              end
            end
          when "03","04"
            if rece.sbt.honkzk == KOREI7
              @errors.push("31760",rece,ho,"HKNJANUM")
            end
          when "07"
            if rece.sbt.honkzk != HONNIN
              @errors.push("31760",rece,ho,"HKNJANUM")
            end
          when "06","63"
              if ho["KIGO"].value.gsub(/[０-９]/,"") != ""
                @errors.push("31770",rece,ho,"KIGO")
              end
          end
        end
      end

      if ho["NUM"].value == ""
        @errors.push("21620",rece,ho,"NUM")
      else
        if ho["NUM"].value =~ /( |　)/
          @errors.push("21640",rece,ho,"NUM")
        end
      end

      if ho["NUM"].value =~ /[０-９]/
      else
        @errors.push("31460",rece,ho,"NUM")
      end

      if ho["HKNJANUM"].value =~ (/^([0-9]{8}|  [0-9]{6}|    [0-9]{4})$/)
      else
        @errors.push("21670",rece,ho,"HKNJANUM")
      end

      if ho["HKNJANUM"].value == "32130213"
        if ho["KIGO"].value != "都" && ho["KIGO"].value != "都任継"
          @errors.push("31710",rece,ho,"KIGO")
        end
        if ho["NUM"].value.split(//).size != 8 ||
           ho["NUM"].value.gsub(/[０-９]/,"") != ""
          @errors.push("31750",rece,ho,"NUM")
        end
      end

      if ["01","02","06","63","33"].include?(ho["HKNJANUM"].value[0,2]) ||
         ho["HKNJANUM"].value == "32130213"
        if ho["KIGO"].value.gsub(/　/,"").strip == ""
          @errors.push("31720",rece,ho,"KIGO")
        end
      end

      if ho["DCKBN"].value  == "1"
        if ho["DCRATE"].value == "" || ( ho["DCRATE"].value.to_i >= 1 and  ho["DCRATE"].value.to_i <= 99 )
        else
          @errors.push("35210",rece,ho,"DCRATE")
        end
        if ho["DCRATE"].value != "" && ho["DCMONEY"].value != ""
          myfooter=sprintf("%s %s",edit_msg(ho,"DCRATE"),edit_msg(ho,"DCMONEY"))
          @errors.push("35280",rece,ho,nil,nil,myfooter)
        end
      else
        if ho["DCRATE"].value != ""
          @errors.push("35140",rece,ho,"DCRATE",nil,edit_msg(ho,"DCKBN"))
        end
        if ho["DCMONEY"].value != ""
          @errors.push("35150",rece,ho,"DCMONEY",nil,edit_msg(ho,"DCKBN"))
        end
      end

      case ho["DCKBN"].value
      when  "1","2","3",""
      else
        @errors.push("35200",rece,ho,"DCKBN")
      end

      if ho["SSKAISU"].value.to_i > re["SRYYM"].nissu * 3
        @errors.push("35310",rece,ho,"SSKAISU")
      end

      if ho["SSKAISU"].value.to_i > ho["JNISSU"].value.to_i * 3
        @errors.push("35340",rece,ho,"SSKAISU",nil,edit_msg(ho,"JNISSU"))
      end

      #---------------------------------------------------------------------------------------------------
      #          フォーマットチェック(HOレコード)
      check_format(rece,ho)
      #---------------------------------------------------------------------------------------------------
    }

    if rece.ho.size > 1
      @errors.push("25470",rece)
    end

    if rece.sbt.exists?
      recesbterr=false
      if rece.sbt.ho_count == 0
        if rece.ho.size == 1
          recesbterr=true
        end
      else
        if rece.ho.empty?
          recesbterr=true
        end
      end
      if rece.sbt.ko_count != rece.ko.size
        recesbterr=true
      end
      if recesbterr
        @errors.push("25521",rece)
      end
    end

    rece.ro.each{|ro|
      @errors.push("23300",rece,ro)
    }

    ko_keys = Array.new
    ko_pty  = 0
    ko_pty_error  = false
    rece.ko.each_with_index{|ko,i|

      if i >= 4
        break
      end


      if ko["JNISSU"].value.strip == ""
        @errors.push(["30320","30330","30350","30360"][i],rece,ko,"JNISSU")
      else
        if re["SRYYM"].nissu != nil && re["SRYYM"].nissu < ko["JNISSU"].value.to_i
          @errors.push(["32020","32030","32120","32130"][i],rece,ko,"JNISSU")
        end
      end

      if rece.sbt.ho_count == 0 && rece.sbt.ko_count == 1
      else
        if KoSort.key?(ko["FTNNUM"].hankaku[0,2]) && KoSort.tandoku?(ko["FTNNUM"].hankaku[0,2])
          @errors.push(["30930","30940","30960","30830"][i],rece,ko,"FTNNUM")
        end
      end

      if ko["FTNNUM"].hankaku[0,2] == "30"
        myfooter=sprintf("%s　%s",edit_msg(re,"RECESBT"),edit_msg(ko,"JKYNUM"))
        if re["RECESBT"].hankaku  =~ /^(1211|1212)$/
          if ko["JKYNUM"].value.strip != ""
            @errors.push("21690",rece,ko,"FTNNUM",nil,myfooter)
          end
        else
          @errors.push("21690",rece,ko,"FTNNUM",nil,myfooter)
        end
      else
        if ko["JKYNUM"].value.strip == ""
          @errors.push("21711",rece,ko,"JKYNUM")
        elsif ko["JKYNUM"].value.gsub(/0/,"") == ""
          @errors.push(["31520","31550","31580","31590"][i],rece,ko,"JKYNUM")
        end
      end

      ko_key = sprintf("%s%s",ko["FTNNUM"].value,ko["JKYNUM"].value)

      if ko_keys.include?(ko_key)
        myfooter=sprintf("%s　%s",edit_msg(ko,"FTNNUM"),edit_msg(ko,"JKYNUM"))
        @errors.push(["00000","30840","30860","30810"][i],rece,ko,nil,nil,myfooter)
      end

      if ko["SSKAISU"].value.to_i > re["SRYYM"].nissu * 3
        @errors.push(["35320","35330","35410","35420"][i] ,rece,ko,"SSKAISU")
      end

      if ko["SSKAISU"].value.to_i > ko["JNISSU"].value.to_i * 3
        @errors.push(["35350","35360","35430","35440"][i] ,rece,ko,"SSKAISU",nil,edit_msg(ko,"JNISSU"))
      end

      ko_keys << ko_key

      if ko_pty_error == false
        if KoSort.key?(ko["FTNNUM"].value[0,2])
          if ko_pty > KoSort.pty(ko["FTNNUM"].value[0,2])
            ko_pty_error = true
          end
          ko_pty = KoSort.pty(ko["FTNNUM"].value[0,2])
        end
      end

      #---------------------------------------------------------------------------------------------------
      #          フォーマットチェック(KOレコード)
      check_format(rece,ko)
      #---------------------------------------------------------------------------------------------------
    }

    if ko_pty_error == true
      @errors.push("30980",rece)
    end

  end



  def check_sy_tekiyo(ir,rece)

    re=rece.re.first
    myoldest_sryymd=nil
    mybyo_stdays=Array.new.fill(false,1..31)
    mysyosin_NG_days=Array.new.fill(false,1..31)
    myten=Array.new(5,0)
    myssmoney=Array.new(5,nil)

    if rece.sy.size >= 100
      @errors.push("22020",rece)
    end

    if rece.sy.empty?
      @errors.push("25540",rece)
    end

    rece.sy.each{|sy|
      if sy["SRYYMD"].wtos != ""
        if re["SRYYM"].wtos[0,6].strip != ""
          if re["SRYYM"].wtos[0,6] < sy["SRYYMD"].wtos[0,6]
            @errors.push("32060",rece,sy,"SRYYMD")
          elsif re["SRYYM"].wtos[0,6]  == sy["SRYYMD"].wtos[0,6]
            mybyo_stdays[sy["SRYYMD"].wtos[6,2].to_i] = true
            if sy["TENKIKBN"].value.strip == ""
              mysyosin_NG_days.fill(true, sy["SRYYMD"].wtos[6,2].to_i + 1..31)
            end
          else
            if sy["TENKIKBN"].value.strip == ""
              mysyosin_NG_days.fill(true,1..31)
            end
          end
        end
        if re["BIRTHDAY"].wtos != ""
          if re["BIRTHDAY"].wtos > sy["SRYYMD"].wtos
            @errors.push("32210",rece,sy,"SRYYMD")
          end
        end
        if myoldest_sryymd.nil? || myoldest_sryymd > sy["SRYYMD"].wtos
           myoldest_sryymd = sy["SRYYMD"].wtos
        end
      else
        @errors.push("32070",rece,sy,"SRYYMD")
      end

      if sy["TENKIKBN"].value =~ /^[1234]$/
      else
        @errors.push("32080",rece,sy,"TENKIKBN")
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

    if rece.tekiyo.empty?
      @errors.push("25550",rece,nil)
    end

    srtSrykbn="00"

    rece.tekiyo.each{|tekiyo|
      myzaikaisu=nil
      myreceline=0
      myzaiday=Hash.new
      myzailine=Hash.new
      flg_plus  = false
      flg_minus = false

      ksnsort = KsnSort.new

      tekiyo.zai.each_with_index{|zai,zai_idx|
        case zai["RECID"].value
        when "SI","IY","TO","CO"
          zai.set_tbl_tensu(select_tensu(@hospnum,zai["SRYCD"].value,re["SRYYM"].value))
        else
          zai.set_tbl_tensu(nil)
        end
      }

      tekiyo.zai.each_with_index{|zai,zai_idx|

        tbl_tensu = zai.tbl_tensu

        myfooter_recesbt=edit_msg(re,"RECESBT")

        if rece.sbt.exists?
          if zai.hash.key?("FTNKBN")
            if FTNKBN.key?(zai["FTNKBN"].hankaku)
              FTNKBN[zai["FTNKBN"].hankaku].each_with_index{|ftn,i|
                if ftn == 1
                  if rece.sbt.sikaku[i] == 0
                    @errors.push("23020",rece,zai,"FTNKBN",nil,myfooter_recesbt)
                    break
                  end
                end
              }
            else
              @errors.push("23020",rece,zai,"FTNKBN",nil,myfooter_recesbt)
            end
          end

          if zai.hash.key?("SRYKBN") && zai["SRYKBN"].value != ""

            flg_plus  = false
            flg_minus = false

            if  SRYKBN.key?(zai["SRYKBN"].hankaku)
              if SRYKBN[zai["SRYKBN"].hankaku] == NYUGAI || SRYKBN[zai["SRYKBN"].hankaku] == rece.sbt.nyugai
                if zai["SRYKBN"].value < srtSrykbn
                  @errors.push("23060",rece,zai,"SRYKBN",nil,"前回診療識別［#{srtSrykbn}］")
                end
                srtSrykbn = zai["SRYKBN"].value
              else
                @errors.push("23030",rece,zai,"SRYKBN",nil,myfooter_recesbt)
              end
            else
              @errors.push("23030",rece,zai,"SRYKBN",nil,myfooter_recesbt)
            end
          end

          case zai["RECID"].value
          when  "SI","IY","TO","CO"
            tbl_tensu=select_tensu(@hospnum,zai["SRYCD"].value,re["SRYYM"].value)
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
                when  "SI"

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
                      if re["SRYYM"].value >= H200401
                        if rece.age  < 65 || rece.sbt.nyugai == GAIRAI
                          if tbl_tensu["NYUTENTTLKBN"].to_i >= 972
                            myfooter=sprintf("診療識別［%s］ %s",tekiyo.srykbn,tbl_tensu["NAME"])
                            @errors.push("34960",rece,zai,nil,nil,myfooter)
                          end
                        end
                      end
                    else
                      myfooter=sprintf("診療識別［%s］ %s",tekiyo.srykbn,tbl_tensu["NAME"])
                      @errors.push("34910",rece,zai,nil,nil,myfooter)
                    end
                  else
                    if tbl_tensu["NYUTENTTLKBN"].to_i >= 970 and tbl_tensu["NYUTENTTLKBN"].to_i <= 975
                      myfooter=sprintf("診療識別［%s］ %s",tekiyo.srykbn,tbl_tensu["NAME"])
                      @errors.push("34900",rece,zai,nil,nil,myfooter)
                    end
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
                          @errors.push("46410",rece,zai,"CHUKSNCD",nil,tbl_tensu["NAME"])
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

                  if tekiyo.srykbn == "97"
                      myfooter=sprintf("診療識別［%s］ %s",tekiyo.srykbn,tbl_tensu["NAME"])
                      @errors.push("34950",rece,zai,nil,nil,myfooter)
                  end

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
                   if tbl_tensu["TANKA"].to_i == 0
                     @errors.push("33130",rece,zai,"TANKA",nil,tbl_tensu["NAME"])
                   end
                  end

                  if zai["SRYCD"].value == "777770000"
                    if zai["NAME"].value.gsub(/　/,"").strip == "" && zai["INFO"].value.gsub(/　/,"").strip == ""
                     @errors.push("33160",rece,zai,"NAME",nil,tbl_tensu["NAME"])
                     @errors.push("33160",rece,zai,"INFO",nil,tbl_tensu["NAME"])
                   end
                  end

                  if tekiyo.srykbn == "97"
                      myfooter=sprintf("診療識別［%s］ %s",tekiyo.srykbn,tbl_tensu["NAME"])
                      @errors.push("34950",rece,zai,nil,nil,myfooter)
                  end

                when  "CO"
                end
              end
            else
              @errors.push("23040",rece,zai,"SRYCD")
            end
          end
          mysrykaisu=0
          if zai["RECID"].value == "NI"
            if re["SRYYM"].wtos >= H240401
               @errors.push("23200",rece,zai)
            end
          else
            zai.hash.each{|key,row|
              if key =~ /^COMCD/
                mycommoji=sprintf("%s%s","COMMENT",key.sub(/^COMCD/,""))
                if row.value  != ""
                  com_tensu=select_tensu(@hospnum,row.value,re["SRYYM"].value)
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
                    when "40"
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
                    @errors.push("23130",rece,zai,key)
                  end
                  mysryymd = sprintf("%s%s",re["SRYYM"].wtos[0,6],key.sub(/DAY/,""))

                  if re["BIRTHDAY"].wtos != "" && re["BIRTHDAY"].wtos > mysryymd
                    @errors.push("32220",rece,zai,key,nil,tbl_tensu["NAME"])
                  end

                  if re["SRYYM"].wtos[0,6] != ""
                    day_idx = key.sub(/DAY/,"").to_i
                    if myoldest_sryymd != nil &&  mysryymd < myoldest_sryymd
                      @errors.push("44550",rece,zai,key,nil,tbl_tensu["NAME"])
                    end
                    if tbl_tensu.empty?
                    else
                      if tbl_tensu["JITUDAY"].to_i == 2 && tbl_tensu["DAYCNT"].to_i == 1
                        if mysyosin_NG_days[day_idx] == true || mybyo_stdays[day_idx] == false
                          myheader=sprintf("%s算定日%d日",tbl_tensu["NAME"],key.sub(/DAY/,"").to_i)
                          @errors.push("46340",rece,zai,nil,myheader)
                        end
                      end
                    end
                  end
                end
                if myzaiday.key?(key)
                  if  myzaiday[key] != row.value
                    myfooter=sprintf("レセプト内レコード番号［%d］の算定日の回数［%s］",myzailine[key],myzaiday[key])
                    @errors.push("23140",rece,zai,key,nil,myfooter)
                  end
                else
                  myzaiday[key]=row.value
                  myzailine[key]=zai.receline
                end
                mysrykaisu += row.value.to_i

                if row.value  != ""
                  if wtos(sprintf("%s%s",re["SRYYM"].value,key.sub(/DAY/,""))) == ""
                    @errors.push("23170",rece,zai,key)
                  end
                end
              end
            }
            #---------------------------------------------------------------------------------------------------
            #                   フォーマットチェック(SI,IY,TO,COレコード)
            case  zai["RECID"].value
            when  "SI"
              check_format(rece,zai)
            when  "IY"
              check_format(rece,zai)
            when  "TO"
              check_format(rece,zai)
            when  "CO"
              check_format(rece,zai)
            end
            #---------------------------------------------------------------------------------------------------
          end

          case zai["RECID"].value
          when  "SI","IY","TO"

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

              if FTNKBN.key?(zai["FTNKBN"].hankaku)
                FTNKBN[zai["FTNKBN"].value].each_with_index{|ftn,i|
                  if ftn == 1
                    if  tekiyo.srykbn != "97"
                      if flg_minus == false
                        myten[i] += zai["KAISU"].value.to_i * zai["TEN"].value.to_i
                      else
                        myten[i] -= zai["KAISU"].value.to_i * zai["TEN"].value.to_i
                      end
                    else
                      if tbl_tensu.empty?
                      else
                         case tbl_tensu["NYUTENTTLKBN"].to_i
                         when 970, 972, 974
                           if myssmoney[i].nil?
                             myssmoney[i]  = zai["KAISU"].value.to_i * zai["TEN"].value.to_i
                           else
                             myssmoney[i] += zai["KAISU"].value.to_i * zai["TEN"].value.to_i
                           end
                         end
                      end
                    end
                  end
                }
              end
            end

            if zai["KAISU"].value.to_i == 0
              @errors.push("23110",rece,zai,"KAISU")
            end
            if zai["KAISU"].value.to_i != mysrykaisu
              myfooter=sprintf("算定日情報（回数）の合計値［%s］",mysrykaisu)
              @errors.push("23150",rece,zai,"KAISU",nil,myfooter)
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

            flg_tensu_required = false
            case
            when ( tekiyo.zai[zai_idx + 1] == nil ) ||
                 ( tekiyo.zai[zai_idx + 1].hash.key?("SRYKBN") == nil ) ||
                 ( tekiyo.zai[zai_idx + 1]["SRYKBN"].value != "" )
              flg_tensu_required = true
            else
              if zai["RECID"].value == "SI"
                case
                when ( tbl_tensu["JITUDAY"].to_i == 4 && tbl_tensu["DAYCNT"].to_i == 0 ) ||
                     ( tbl_tensu["TENSIKIBETU"].to_i == 4 || tbl_tensu["TENSIKIBETU"].to_i == 7 )
                  flg_tensu_required = true
                when ( tekiyo.srykbn =~ /^(11|12|13|14)$/)
                  case tbl_tensu["KOKUJISKBKBN1"].to_i
                  when 1,3,5,7
                    if tekiyo.zai[zai_idx + 1].tbl_tensu.empty?
                      flg_tensu_required = true
                    else
                      if tekiyo.zai[zai_idx + 1].tbl_tensu["KOKUJISKBKBN1"].to_i == 7 
                      else
                        flg_tensu_required = true
                      end
                    end
                  end
                end
              end
            end

            if flg_tensu_required == true
              if @check_level == 1
                if zai["TEN"].value.to_i == 0
                  @errors.push("33070",rece,zai,"TEN",nil,tbl_tensu["NAME"])
                end
              else
                if zai["TEN"].value == ""
                  @errors.push("33071",rece,zai,"TEN",nil,tbl_tensu["NAME"])
                end
              end
            end

          end

          if zai["RECID"].value == "CO"

            myfooter=""
            if tbl_tensu.empty?
            else
              myfooter=tbl_tensu["NAME"]
            end

            case zai["SRYCD"].value[1,2]
            when "10" , "30"
              if zai["DATA"].value.gsub(/　/,"").strip == ""
                @errors.push("34380",rece,zai,"DATA",nil,myfooter)
              end
            when "40"
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

          if tekiyo.srykbn =~ /^(01|99)$/
            if zai["RECID"].value == "CO"
            else
              @errors.push("23810",rece,zai,"RECID")
            end
          end
        end
      }
    }

    myten_total = 0
    myten.each{|ten| myten_total += ten.to_i}
    if myten_total < 0
      myfooter=sprintf("適用情報レコードを合計した点数［%d］",myten_total)
      @errors.push("33800",rece,nil,nil,nil,myfooter)
    end

    if rece.ho.empty?
    else
      if rece.ho.first["TEN"].value.to_i != myten[0]
        myfooter=sprintf("適用情報レコードの点数合計［%d］",myten[0])
        @errors.push("45010",rece,rece.ho.first,"TEN",nil,myfooter)
      end

      if myten[0] < 0
        myfooter=sprintf("主保険の適用情報レコードを合計した点数［%d］",myten[0])
        @errors.push("33800",rece,rece.ho.first,nil,nil,myfooter)
      end

      if myssmoney[0] != nil && rece.ho.first["SSMONEY"].value.to_i != myssmoney[0]
        myfooter=sprintf("適用情報レコードの食事療養・生活療養合計［%d］",myssmoney[0])
        @errors.push("45340",rece,rece.ho.first,"TEN",nil,myfooter)
      end
    end

    rece.ko.each_with_index{|ko,i|

      if ko["TEN"].value.to_i != myten[i + 1]
        myfooter=sprintf("適用情報レコードを合計した点数［%d］",myten[i+1])
        @errors.push(["45020","45030","45250","45260"][i],rece,ko,"TEN",nil,myfooter)
      end

      if myten[i + 1] < 0
        myfooter=sprintf("第%s公費の適用情報レコードを合計した点数［%d］",["一","二","三","四"][i],myten[i+1])
        @errors.push("33800",rece,ko,nil,nil,myfooter)
      end

      if myssmoney[i + 1] != nil && ko["SSMONEY"].value.to_i != myssmoney[i + 1]
        myfooter=sprintf("適用情報レコードの食事療養・生活療養合計［%d］",myssmoney[i + 1])
        @errors.push(["45350","45360","45640","45650"][i],rece,ko,"SSMONEY",nil,myfooter)
      end
    }

  end

  def check_sj(ir,rece)

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

  def check_sai(ir,rece)

    re=rece.re.first

    if rece.sai.empty?
    else
      if rece.sai.last.line != rece.rows.last.line ||
        rece.sai.size != (rece.sai.last.line - rece.sai.first.line + 1)
        @errors.push("21111",rece)
      end
      if rece.sai.last.rows.size != 5
        @errors.push("21120",rece,rece.sai.last)
      end
      if rece.sai.last["LINE"].value.bytesize <= 5 &&
        rece.sai.last["LINE"].num != nil
      else
        @errors.push("21130",rece,rece.sai.last,"LINE")
      end
      if rece.sai.last["BRANCH"].value.bytesize <= 3 &&
        rece.sai.last["BRANCH"].num != nil
      else
        @errors.push("21140",rece,rece.sai.last,"BRANCH")
      end
      if rece.sai.last["DATA01"].value != "RC"
        @errors.push("21150",rece,rece.sai.last,"DATA01")
      end
      if rece.sai.last["DATA02"].value =~ /^([0-9]|[A-Z]|[a-z])+$/ && rece.sai.last["DATA02"].value.bytesize <= 100
      else
        @errors.push("21160",rece,rece.sai.last,"DATA02")
      end
      if re["SELNUM"].value == ""
        @errors.push("21200",rece,re,"SELNUM")
      else
        if rece.sai.first["DATA01"].value == "MN"
          if rece.sai.first["DATA04"].value != re["SELNUM"].value
            @errors.push("21200",rece,re,"SELNUM",nil,"履歴管理ブロック検査番号［#{rece.sai.first["DATA04"].value}］")
          end
        else
          if rece.sai.first["DATA19"].value != re["SELNUM"].value
            @errors.push("21200",rece,re,"SELNUM",nil,"履歴管理ブロック検査番号［#{rece.sai.first["DATA19"].value}］")
          end
        end
      end
    end
  end

  def main

    @hospnum,@infile,@outfile,@errlog,@check_level=ARGV[0].split(",")

    @db = PandaDB.new
    @db.execFunction("DBOPEN")
    @db.execFunction("DBSTART")
    @info=Recinfo.new
    @errors=Receden_error.new

    r=Receden.new(@infile)
    check_lv1(r)
    if @errors.empty?
      check_receipt(r)
    end
    @errors.write(@outfile)

    @db.execFunction("DBCOMMIT")
    @db.execFunction("DBDISCONNECT")

  rescue => err
    open(@errlog,"w"){|f|
      f.print "#{err.class}\n#{err.message}\n#{err.backtrace.join("\n")}"
    }
    raise "#{err.message}\n#{err.backtrace.join("\n")}"
  end

end
