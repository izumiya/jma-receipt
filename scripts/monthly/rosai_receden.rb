#!/usr/bin/ruby

require "csv"
require "iconv"
require "monthly/rosai_receden_common"

$KCODE = 'e'

class Receden < Receden_common

  attr_reader :ir,:rs,:receipt,:info

  def initialize(receden_file)

    @ir=Array.new
    @rs=Array.new
    @sai=Array.new
    @etc=Array.new
    @receipt=Array.new
    @info=Recinfo.new

    block =Array.new
    j=0
    k=0
    receline=0
    open(receden_file) {|f|
      f.each_with_index{|data,line|
        csv=Csvline.new
        csv.line=line + 1
        csv.rows=data.chomp.split(",",-1)
        case csv.rows[0]
        when "RE","RR","SY","RI","IY","TO","CO","SJ"
          if block[0] == nil
             block[0] = Array.new
             receline=0
          end
          if csv.rows[0] == "RE"
            if block[j].size > 0
               j += 1
               block[j] = Array.new
               receline=0
            end
          end
          receline += 1
          csv.receline = receline
          block[j] << csv
        else
          receline=0
          csv.receline = receline
          case csv.rows[0]
          when "IR"
            @ir << csv_to_hash(@info["IR"],csv)
          when "RS"
            @rs << csv_to_hash(@info["RS"],csv)
          else
            @etc << csv_to_hash(@info["ETC"],csv)
          end
        end
      }
    }
    block.each{|recedata|
        @receipt << Recept.new(recedata,@info)
    }
  end
end

class Recept < Receden_common

  attr_reader :re,:rr,:sy,:tekiyo,:sj,:rows,:sbt,:age,:sryym,:kyufu_key

  def initialize(recedata,info)

    @re=Array.new
    @rr=Array.new
    @sy=Array.new
    @sj=Array.new

    @tekiyo=Array.new

    @rows=Array.new
    @sbt=Recesbt.new("")
    @age=nil
    @sryym=""
    @kyufu_key=""

    block =Array.new

    i=0
    j=0
    recedata.each{|csv|

      @rows << csv

      case csv.rows[0]
      when "RI","IY","TO","CO"
        if block[0] == nil
           block[0] = Array.new
        end
        if csv.rows[1].strip != ""
          if block[j].size > 0
             j += 1
             block[j] = Array.new
          end
        end
        block[j] << csv
      when "RE"
        @re << csv_to_hash(info["RE"],csv)
      when "RR"
        @rr << csv_to_hash(info["RR"],csv)
      when "SY"
        @sy << csv_to_hash(info["SY"],csv)
      when "SJ"
        if @sj[0] == nil
          @sj[0]=Array.new
        end
        if csv.rows[1].strip != ""
          if @sj[i].size >  0
            i += 1
            @sj[i]=Array.new
          end
        end
        @sj[i] << csv_to_hash(info["SJ"],csv)
      end
    }

    if @re.empty?
    else

      @sryym=@re[0]["SKYINF"].value[5,5]
      if wtos(@sryym) != ""
        @age = ((wtos(@sryym).to_i - @re[0]["BIRTHDAY"].wtos.to_i) / 10000).to_i
      end

    end

    if @rr.empty?
    else
      @sbt=Recesbt.new(@rr[0]["FORMSBT"].value)
      if @re.size >= 1 && @sbt.exists?
        if @sbt.form == TANKI
          @kyufu_key ="TANKI_:#{@rr[0]["TANKINUM"].value.strip}," +
                      "#{@rr[0]["KANANAME"].value.strip}," +
                      "#{@re[0]["BIRTHDAY"].value.strip}," +
                      "#{@rr[0]["SHOBYOYMD"].value.strip}," +
                      "#{@rr[0]["RYOSTYMD"].value.strip}," +
                      "#{@rr[0]["RYOEDYMD"].value.strip}," +
                      "#{@rr[0]["JNISSU"].value.strip}," +
                      "#{@rr[0]["GOKEI"].value.strip}," +
                      "#{@rr[0]["FORMSBT"].value.strip}"
        else
          @kyufu_key ="CHOKI_#{@rr[0]["KANANAME"].value.strip}," +
                      "#{@rr[0]["CHOKINUM"].value.strip}," +
                      "#{@rr[0]["RYOSTYMD"].value.strip}," +
                      "#{@rr[0]["RYOEDYMD"].value.strip}," +
                      "#{@rr[0]["JNISSU"].value.strip}," +
                      "#{@rr[0]["GOKEI"].value.strip}," +
                      "#{@rr[0]["FORMSBT"].value.strip}"
        end
      end
    end

    block.each{|zaidata|
        @tekiyo << Tekiyo.new(zaidata,info)
    }
  end


end

class Tekiyo < Receden_common

  attr_reader :zai,:srykbn

  def initialize(zaidata,info)

    @zai=Array.new
    @srykbn=""

    zaidata.each {|csv|
      case csv.rows[0]
      when "RI"
          @zai << csv_to_hash(info["RI"],csv)
      when "IY"
          @zai << csv_to_hash(info["IY"],csv)
      when "TO"
          @zai << csv_to_hash(info["TO"],csv)
      when "CO"
          @zai << csv_to_hash(info["CO"],csv)
      end
    }

    if @zai.first.hash.key?("SRYKBN")
      @srykbn = @zai.first["SRYKBN"].value
    end

  end
end