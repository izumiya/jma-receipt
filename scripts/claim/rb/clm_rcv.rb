#!/usr/bin/env ruby

# claim test Rcv server script
#   args  0:[port]
# 
# version 1.3.0
#                  '01-10-30 by Ymmt
# version 1.4.0 �ޥ������å��б�
#                  '02-11-12 by hiki
# version 1.4.1 �������ʣ�������б�
#               ����ˡ��������٤˥�����ȥ��åפ�����ֹ���ղä��Ƥ���
#                  '03-10-15 by hiki
# version 1.4.2 �������³���ڤ줿�ꡢnmap���ޥ�ɤؤ��к�
#                  '05-02-08 by hiki
# version 1.4.3 fix correction of socket opening.
#                  '2011-02-09
# version 1.4.4 �����åȥ����ץ��Ի���̵�¥롼�פ���.
#               �������Ϥ��ɲ�.
#                  '2011-11-30

$DEBUG = false

Dir.chdir(File.dirname(__FILE__))

require 'socket'
require 'xml_valid.rb'

$default_port = "8210"

if ARGV.length == 0
  $port = $default_port
else
  $port = ARGV[0]
end

$dtdfl = "../../../scripts/claim/dtd/MML1014_euc.dtd"
$file_path = "/var/tmp"
$logfl = "/tmp/claim_rcv.log"

$sh_path_name = "../../../scripts/claim/HL03.sh"
$seq_put = 0        # ��Ǽ���˥��åȤ��������ֹ�
$seq_exec = 0       # �¹��漱���ֹ�
$seq_max = 32767    # �����ֹ�κ�����
$thr_sleeptime = 1  # ����åɤν���Ԥ�����sleep����
$fname_seq = 0      # ����������(�ե�����̾�˻��Ѥ���)

# �㳰���饹
class ConnectError < StandardError ; end
class CReadError < StandardError ; end

class FileSockRcv
  def initialize(fl, s)
    @fl_path_name = fl
    @sckt = s
    @eot = 0x04.chr
  end

  def Log(text)
    print text
    open($logfl, "a") do |log|
      log.puts("SockLog: #{text}")
    end
  end

  def start
    open(@fl_path_name, "wb") do |f|
      buf = ""
      begin
        while @eot != (buf = @sckt.read(1))
          if buf == nil
            # �����å���³���ڤ줿
            Log("Connect Error: read socket data is nil")
            raise ConnectError, 'read socket data is nil.'
          end
          print buf
          f.print buf
        end
      rescue ConnectError
        # �������³���ڤ줿���ؤ��б�
        Log("Connect Error")
        raise ConnectError, ''
      rescue
        # nmap���ޥ�ɤؤ��б�
        Log("CRead Error: socket read error")
        raise CReadError, 'socket read error.'
      end
    end
  end
end

class SndRsp
  def initialize(s)
    @sckt = s
    @ack = 0x06.chr
    @nak = 0x15.chr
  end

  def ok
    @sckt << @ack
  end

  def ng
    @sckt << @nak
  end
end

# �ޥ������åɻ��˼¹Ԥ��륯�饹
class ExecThreadMain
  # ���󥹥ȥ饯��
  #
  #   ����
  #     seq      <= ��������åɤΥ������󥷥���ֹ�(0��32767)
  #     sh_path  <= ��ư������Υե�ѥ�̾
  #     xml_path <= �ɤ߹���XML�ե�����Υե�ѥ�̾
  #     out_file <= ���ϥƥ����ȥե�����̾
  #
  #   ����
  #     $seq_put���ͤϡ�����åɤ��ͤ��Ϥ��Ƥ��饫����ȥ��åפ��Ƥ���������
  #
  def initialize(seq, sh_path, xml_path, out_file)
    # �������󥷥���ֹ�򥯥饹�ѿ��ΰ����¸
    @thr_seq = seq
    @sh_pathname = sh_path
    @xml_pathname = xml_path
    @out_filename = out_file
  end


  # �ǥ����ɥᥤ�����
  def decode_main
    `#{@sh_pathname} #{@xml_pathname} #{@out_filename}`
  end


  # �¹��Ԥ��ؿ�
  # ��Ǽ�����ֹ�ȼ¹Լ����ֹ����Ӥȥ������Ƚ����ȳ�Ǽ�����ֹ�Υ�����ȥ��å�
  def exec_start
    while @thr_seq != $seq_exec
      # ����Ԥ�
      sleep $thr_sleeptime
    end
  end


  # �¹Դ�λ�ؿ�
  # �¹Լ����ֹ�Υ�����ȥ��å�
  def exec_end
    $seq_exec += 1
    if $seq_exec > $seq_max
      $seq_exec = 0
    end
  end


  # ����åɥᥤ�����
  # �������顢�缡�¹��Ԥ��������¹Ԥ�ƤӽФ�(public)
  def main
    exec_start  # ����Ԥ�
    decode_main # �缡�¹�
    exec_end    # ������λ����
  end

  public  :main
  private :decode_main
  private :exec_start
  private :exec_end
end

class ClaimRcv
  def initialize
    require 'date'
    @claim_log = "/var/tmp/claim_err_#{Date.today.to_s}.log"
  end

  def file_chk(fl)
    if File.exists?(fl)
      ans = fl + " exists. Overwrite!"
    else 
      ans = fl + " is New file!"
    end
  end

  def make_file_name
    name_A = "claim_rcv_" + Time.now.strftime("%m%d_%H%M%S")
    name_B = "_" + sprintf("%02d", $fname_seq) + ".xml"
    flname = name_A + name_B
    $fname_seq += 1
    if $fname_seq > 99
      $fname_seq = 0
    end
    return flname
  end

  def socket_thread(sock)
    Log("Client login\n")

    file_path_name = File.join($file_path, make_file_name)
    Log(file_chk(file_path_name) + "\n")

    rcvbuf = FileSockRcv.new(file_path_name, sock)
    ans = SndRsp.new(sock)
    
    Log("Start Receiving File --------------------------\n")

    begin
      rcvbuf.start
    rescue ConnectError, CReadError
      Log("Connection Error\n")
      Log("Client disconnects\n")
      sock.close
      sock = nil
      Log("temporary file delete.\n")
      File.delete(file_path_name)
    else
      Log("Complete Receiving File -----------------------\n")
    
      # convert claim data J-code to UTF8
      Log("Convert to UTF-8\n")
      u8_file = file_path_name.gsub(/.xml$/, "_u8.xml")
      `ruby xml_jcnv.rb #{file_path_name} tou8 -f > #{u8_file}`  
      file_path_name = u8_file

      # check claim data and send respons to client
      Log("Claim valid check\n")
      if parser_check(file_path_name, $dtdfl, $logfl)
        Log("Send [ack] to client\n")
        ans.ok
        valid_check_flg = true
      else
        Log("Send [nak] to client\n")
        ans.ng
        valid_check_flg = false
      end

      #  kick shell script ( decode(ruby) and cobol) )
      if valid_check_flg
        out_file = file_path_name.gsub(/_u8.xml$/, ".txt")
        Log("Decode claim data to #{out_file} and kick COBOL\n")
        Log("#{$sh_path_name} #{file_path_name} #{out_file}\n\n")

        # ����åɤ���������
        thr_execflg = 0
        #Thread.start {
          if $DEBUG
            Log("Thread Start[" + String($seq_put + 1) + "]\n")
            Log("Thread Start[" + String($seq_put + 1) + "]\n")
            thr_start_time = ''; thr_end_time = ''
            thr_start_time = Time.now.strftime("%H:%M:%S")
          end

          thr = nil
          # ����å������ᥤ�����
          thr = ExecThreadMain.new($seq_put, $sh_path_name, file_path_name, out_file)
          $seq_put += 1
          if $seq_put > $seq_max
            $seq_put = 0
          end

          thr_execflg = 1
          # �ᥤ�����
          thr.main

          if $DEBUG
            thr_end_time = Time.now.strftime("%H:%M:%S")
            Log("Thread End[" + String($seq_exec) + "] ")
            Log("[" + thr_start_time + "��" + thr_end_time + "]\n")
            Log("End Sequence NO. = [" + String($seq_put) + "]\n")
          end
        #}

        # ����å������Υ������Ƚ���
        while thr_execflg == 0
          sleep 1
        end
      end
    end
  end

  def Log_Save(claim_log=@claim_log)
    File.open(claim_log, "a+") do |file|
      STDOUT.reopen(file)
      STDERR.reopen(file)
    end
  end

  def Log(text)
    print text
    open($logfl, "a") do |log|
      log.puts("Log: #{text}")
    end
  end

  def Reception
    begin
      Log("server:>> ")
      @gsock = TCPServer.open($port)

      Log("No #{$port} port open ")
      Log("[" + Time.now.strftime("%H:%M:%S") + "]\n")
      Log("Waiting...\n")

      while true
        Thread.start(@gsock.accept) do |sock|
          socket_thread(sock)
          Log("Close port [" + Time.now.strftime("%H:%M:%S") + "]\n")
          Log("Client disconnects\n")
          sock.close if sock != nil
        end
      end
    rescue Errno::EADDRINUSE => error
      Log("Error:#{error}\n")
    rescue => error
      Log("Error:#{error}\n")
      retry
    end
    @gsock.close if @gsock != nil
  end
end

if __FILE__ == $0
  claim = ClaimRcv.new
  claim.Reception
end
