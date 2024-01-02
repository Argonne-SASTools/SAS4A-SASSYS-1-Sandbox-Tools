#===============================================================================
#
# This file is part of the SAS4A-SASSYS-1-Sandbox-Tools.
#
# See LICENSE for full details.
#
#===============================================================================
#=========================================================================================
# SASPy
#
# Author: D. J. OGrady and T. H. Fanning
# Argonne National Laboratory
#=========================================================================================
import os, re
import numpy as np


class FortFile(object):
    """
    FortFile is a simple class to encapsulate reading Fortran binary records.

    :param object: Binary file created by Fortran.
    :return: A wrapped binary file that can be iterated.
    """

    def __init__(self,name):
        self.file = open(name,'rb')

    def rewind(self):
        self.file.seek(0)

    def read(self):
        from struct import unpack
        data = None
        len = self.file.read(4)
        if len:
            len, = unpack('I',len)
            data = self.file.read(len)
        end = self.file.read(4)
        if end:
            end, = unpack('I',end)
        if len != end:
            raise IOError('invalid Fortran record length')
        return data

    def __iter__(self):
        return self;
    def __next__(self):
        result = self.read()
        if result is None:
            raise StopIteration
        return result


def readVisual(dat):
    """
    readVisual is a function that processes a SAS4A/SASSYS-1 VISUAL.sasdata file and returns a dictionary containing the channel results.

    :param dat: Path to a VISUAL.sasdata file.
    :return: Dictionary containing axial channel results.
    """

    from struct import Struct
    MEG = 1000000

    file = FortFile(dat)
    # Read and parse the first record and determine number of elements (NBNTOT)

    rec = file.read()
    header = Struct('%is' %(4+4))
    print(header.unpack_from(rec)[0].decode('utf-8'))
    body = Struct('%ii' %(2))
    #print(body.unpack_from(rec,header.size))
    header2 = Struct('%is' %(8+8))
    print(header2.unpack_from(rec,header.size+body.size)[0].decode('utf-8'))

    rec = file.read()
    header = Struct('%is' %(72+8))
    print(header.unpack_from(rec)[0].decode('utf-8'))
    rec = file.read()
    header = Struct('%is' %(72+8))
    print(header.unpack_from(rec)[0].decode('utf-8'))

    s_f = Struct('d')
    s_int = Struct('i')

    dict = {}
    dict['time'] = []

    for rec in file:
        dict['time'].append(s_f.unpack_from(rec)[0])
        NDATA = s_int.unpack_from(rec,s_f.size)[0]
        i = 0
        while (i < NDATA) :
            i += 1
            rec = file.read()

            I1 = s_int.unpack_from(rec)[0] # Channel and data index
            I2 = s_int.unpack_from(rec,s_int.size)[0] # MZ
            body = Struct('%id' %(I2))

            if (I1 < MEG): # Averagea coolant temperature
                ich = I1
                first = True
                key = 'AvgCoolant'
                if ich not in dict.keys():
                    dict[ich] = {}
                    dict[ich][key] = []
                dict[ich][key].append(body.unpack_from(rec,s_int.size*2)[:])
            elif (I1 == 4 * MEG + ich ): # Inner Structure
                key = 'InnerStruct'
                if key not in dict[ich].keys():
                    dict[ich][key] = []
                dict[ich][key].append(body.unpack_from(rec,s_int.size*2))
            elif (I1 == 5 * MEG + ich ): # Inner Structure
                key = 'OuterStruct'
                if key not in dict[ich].keys():
                    dict[ich][key] = []
                dict[ich][key].append(body.unpack_from(rec,s_int.size*2))
            elif (I1 == MEG + ich ): # Inner Cladding
                key = 'CladInner'
                if key not in dict[ich].keys():
                    dict[ich][key] = []
                dict[ich][key].append(body.unpack_from(rec,s_int.size*2))
            elif (I1 == 2 * MEG + ich ): # Mid Cladding
                key = 'CladMid'
                if key not in dict[ich].keys():
                    dict[ich][key] = []
                dict[ich][key].append(body.unpack_from(rec,s_int.size*2))
            elif (I1 == 3 * MEG + ich ): # Outer Cladding
                key = 'CladOuter'
                if key not in dict[ich].keys():
                    dict[ich][key] = []
                dict[ich][key].append(body.unpack_from(rec,s_int.size*2))
            elif (I1 == 10 * MEG + ich ): # Average Fuel
                key = 'AvgFuel'
                if key not in dict[ich].keys():
                    dict[ich][key] = []
                dict[ich][key].append(body.unpack_from(rec,s_int.size*2))
            else:
                key = 'Fuel'
                if key not in dict[ich].keys():
                    dict[ich][key] = []
                data = body.unpack_from(rec,s_int.size*2)[:]
                if (first):
                    first = False
                    dict[ich][key].append([data])
                else:
                    dict[ich][key][-1].append(data)


    for ich in dict.keys():
        if ich == 'time':
            dict[ich] = np.array(dict[ich])
        else:
            for key in dict[ich].keys():
                dict[ich][key] = np.array(dict[ich][key])

    return dict


def readControl(dat):
    """
    readControl is a function that processes a SAS4A/SASSYS-1 CONTORL.dat file and returns a dictionary containing the output.

    :param dat: Path to a CONTROL.dat file.
    :return: Dictionary containing control system output.
    """

    from struct import Struct
    file = FortFile(dat)
    # Read and parse the first record and determine number of elements (NBNTOT)
    rec = file.read()
    head = Struct('1i')
    nSigs = head.unpack_from(rec)
    nSigs = nSigs[0]
    body = Struct('%di'%nSigs)
    rec = file.read()
    isig = body.unpack_from(rec)
    rec = file.read()
    itype = body.unpack_from(rec)
    body = Struct('%is' %(nSigs*32))
    rec = file.read()
    stype = body.unpack_from(rec)
    body = Struct('%dd'%(nSigs))
    first = Struct('1i')
    second = Struct('1d')
    istep = []
    time = []
    data = []
    for rec in file:
        istep.append(first.unpack_from(rec))
        time.append(second.unpack_from(rec,first.size))
        rec = file.read()
        data.append(body.unpack_from(rec))
    CSdict = {}
    CSdict['istep'] = np.array(istep)
    CSdict['time'] = np.array(time)
    CSdict['sig'] = {}
    i = 0
    data = np.array(data)
    for i in range(nSigs):
        CSdict['sig'][isig[i]] = {'itype' : itype[i],
                                 'stype' : stype[0][i*32:(i+1)*32-1].decode('utf-8'),
                                 'data' : data[:,i]}
    return CSdict

def readChannel(dat):
    """
    readChannel is a function that processes a SAS4A/SASSYS-1 CHANNEL.dat file and returns a dictionary containing the output.

    :param dat: Path to a CHANNEL.dat file.
    :return: Dictionary containing channel output.
    """

    from struct import Struct

    file = FortFile(dat)
    # Read and parse the first record and determine number of elements (NBNTOT)

    head = Struct('1i')


    rec = file.read()
    nStep = head.unpack_from(rec)[0]
    fStep = nStep
    nChan = 0

    for rec in file:
        nChan += 1
        nStep = head.unpack_from(rec)[0]
        if nStep != fStep : break

    file.rewind()

    channel1 = Struct('3i')
    channel2 = Struct('18d')
    core1 = Struct('2i')
    core2 = Struct('15d')

    dict = {}
    dict['core'] = {}
    for ich in range(1,nChan+1):
        dict[ich] = {}

    core_keys = ['istep','ipower',
        'time',  'dt', 'power', 'period','energy', 'decay',
        'rho_t', 'rho_p',  'rho_d', 'rho_a', 'rho_r', 'rho_cr','rho_v', 'rho_f', 'rho_c'
    ]
    for key in core_keys:
        dict['core'][key] = []

    channel_keys = [
        'zFuel',  'zClad',  'zCool',  'zStruct','FeJ',    'TcoolA', 'Tclad',  'Tfuel',
        'Tsat',   'fMelt',  'CmidT',  'Cmelt',  'Tcool', 'Tstruct', 'Wout',   'Win',
        'Tout',   'Tin'
    ]
    for key in channel_keys:
        for ich in range(1,nChan+1):
            dict[ich][key] = []

    for rec in file:
        for ICH in range(1,nChan):
            a = channel1.unpack_from(rec)
            b = channel2.unpack_from(rec,channel1.size)
            for j, key in enumerate(channel_keys):
                dict[ICH][key].append(b[j])
            rec = file.read()
        a = core1.unpack_from(rec)
        b = core2.unpack_from(rec,core1.size)

        dict['core']['istep'].append(a[0])
        dict['core']['ipower'].append(a[1])
        for j, key in enumerate(core_keys[2:]):
                dict['core'][key].append(b[j])

    for key in core_keys:
        dict['core'][key] = np.array(dict['core'][key])

    for key in channel_keys:
        for ich in range(1,nChan+1):
            dict[ich][key] = np.array(dict[ich][key])

    return dict


# Function to convert PRIMAR4.dat to python dictionary
def readPRIMAR4(dat,res=None):
    """
    readPRIMAR4 is a function that processes a SAS4A/SASSYS-1 PRIMAR4.dat file and returns a dictionary containing the output.
    readPRIMAR4 can attach the correct headers to PRIMAR4.dat data. For PRIMAR4.dat file created during a RESTART, the
    dictionary containing the steady state PRIMAR4 results is required to obtain the the header information

    :param dat: Path to a PRIMAR4.dat file.
    :param res: Dictionary from the containst the steady state PRIMAR4.dat results
    :return: Dictionary containing PRIMAR4 output.
    """
    from struct import Struct

    # Open Fortran binary file
    file = FortFile(dat)
    labels = Make_labels()
    # Read and parse the first record and determine number of elements (NBNTOT)
    rec = file.read()
    head = Struct('3i')
    (ISTEP, IPRSTP, NBNTOT) = head.unpack_from(rec)
    body = Struct('%dd'%NBNTOT)
    if res is None:
        num_ids = Struct('1i').unpack_from(rec, head.size+body.size)
        num_ids = num_ids[0] + 1
        ids  = Struct('%di'%(num_ids)).unpack_from(rec, head.size+body.size)

        header = []
        i=1
        while (i < num_ids):
            val = ids[i]
            INUM  = int(val%10000)
            BNTYP = int((val-INUM)/10000)
            if INUM > 5000:
                INUM = INUM-5000
                i = i+1
                val = ids[i]
                end = val
                for j in range(INUM,end+1):
                    header.append("%s%i)" %(labels[BNTYP],j))
            else:
                header.append("%s%i)" %(labels[BNTYP],INUM))
            i= i+1
    else:
        header = list(res.keys())[1:]

    data = []
    # Read the data file and generate a spreadsheet
    file.rewind()
    for rec in file:
        (ISTEP, IPRSTP, NBNTOT) = head.unpack_from(rec)
        data.append( body.unpack_from(rec,head.size))

    data = np.array(data)

    PMR4_dict = {'TIME'  : data[:,0]}
    for i in range(len(header)):
        PMR4_dict[header[i]] = data[:,i+1]
    return PMR4_dict

def Make_labels():
    """
    Make_labels is a helper function for readPRIMAR4.
    It is designed to read the rst format for the binary input and convert it to a dictionary to be
    used by readPRIMAR4.

    :return: Dictionary containing PRIMAR4.dat label-key pairs.
    """

    junk = np.random.random_sample()
    file = 'PRIMAR%5f.txt' %junk
    with open(file,'w') as fout:
        fout.write("""* - 1
  - ISGL
  - flow, liquid segment
  - FLOSL2(ISGL)
* - 2
  - ISGG
  - flow, gas segment
  - FLOSG4(ISGLG)
* - 3
  - ICH
  - estimated channel inlet
  - CHFLO2(1,ICH)
* - 4
  - ICH
  - estimated channel outlet flow
  - CHFLO2(2,ICH)
* - 5
  - L
  - estimated core flow
  - CORFLE(L)
* - 6
  - L
  - estimated core flow times temperature
  - CORFTE(L)
* - 7
  - L
  - actual integrated channel flow
  - CORCHF(L)
* - 8
  - L
  - channel flow times temperature
  - CORFLT(L)
* - 9
  - ICH+100\*(L-1)
  - actual channel flow, beginning of step
  - FLOCH1(L,ICH)
* - 10
  - ICH+100\*(L-1)
  - coefficients used to estimate the
  - C0FLCH(L,ICH)
* - 11
  - ICH+100\*(L-1)
  - core flow for the next step
  - C1FLCH(L,ICH)
* - 12
  - ICH+100\*(L-1)
  -
  - C2FLCH(L,ICH)
* - 13
  - ICH+100\*(L-1)
  -
  - C3FLCH(L,ICH)
* - 14
  - ICH+100\*(L-1)
  - subassembly inlet or outlet temperature
  - TEXPEL(L,ICH)
* - 15
  - ICH+100\*(L-1)
  - energy of vapor condensing in inlet or outlet plenum
  - ENVAPR(L,ICH)
* - 16
  - ICV
  - liquid pressure for compressible volume ICV
  - PRESL2(ICV)
* - 17
  - ICV
  - gas pressure
  - PRESG2(ICV)
* - 18
  - IPMP
  - pump head for pump IPMP
  - HEADP2(IPMP)
* - 19
  - ICV
  - cover gas interface height
  - ZINTR2(ICV)
* - 20
  - ICV
  - gas volume
  - VOLGC2(ICV)
* - 21
  - ICV
  - total volume, liquid+gas
  - VOLLGC(ICV)
* - 22
  - ICV
  - liquid mass
  - XLQMS2(ICV)
* - 23
  - ICV
  - gas mass
  - GASMS2(ICV)
* - 24
  - ICV
  - liquid temperature
  - TLQCV2(ICV)
* - 25
  - ICV
  - liquid density
  - DNSCV2(ICV)
* - 26
  - ICV
  - wall temperature
  - TWLCV2(ICV)
* - 27
  - ICV
  - gas temperature
  - TGASC2(ICV)
* - 28
  - ISGL+100\*(L-1)
  - liquid segment inlet or outlet temperature
  - TSLIN2(L,ISGL)
* - 29
  - IELL
  - gravity head for element IELL
  - GRAVHD(IELL)
* - 30
  - IELL+400\*(L-1)
  - liquid element temperature
  - TELEM(L,IELL)
* - 31
  - ITGP
  - fraction of a node traversed by Lagrangian slugs In temperature group ITGP
  - FRNDF2(ITGP)
* - 32
  - INOD
  - liquid temperature, node INOD
  - TLNOD2(INOD)
* - 33
  - INOD
  - wall temperature
  - TWNOD2(INOD)
* - 34
  - --
  - outlet plenum density
  - DLHOT
* - 35
  - --
  - inlet plenum density
  - DLCOLD
* - 36
  - ICH
  - inlet temperature
  - TINVAL(ICH)
* - 37
  - --
  - outlet plenum pressure at beginning of time step
  - PXT0
* - 38
  - --
  - time derivative of outlet plenum pressure
  - DPXDT
* - 39
  - IPMP
  - pump speed
  - PSPED2(IPMP)
* - 40
  - --
  - time derivative of inlet plenum pressure
  - DPINDT
* - 41
  - --
  - inlet plenum pressure at beginning of time step
  - PIN
* - 42
  -
  - Not used
  -
* - 43
  -
  - Not used
  -
* - 44
  -
  - Estimated boiling time
  - BOILTM
* - 45
  - --
  - next PRIMAR step size
  - DTPNXT
* - 46
  - ICH
  - coolant re-entry temperature
  - TUPLVL(ICH)
* - 47
  -
  - Not used
  -
* - 48
  - L
  - accumulated error in plenum mass
  - DMSSUM
* - 49
  - L
  - accumulated error in plenum mass times temperature
  - DMTSUM
* - 50
  - INOD
  - temperature of sink for component-to-component heat transfer
  - TSNKND(INOD)
* - 51
  - INOD
  - heat transfer coefficient for component-to-component heat transfer
  - HSNKND(INOD)
* - 52
  - ICV
  - temperature of sink for component-to-component heat transfer
  - TSNKCV(ICV)
* - 53
  - ICV
  - heat transfer coefficient for component-to-component heat transfer
  - HSNKCV(ICV)
* - 54
  - ICV
  - component-to-component heat transfer rate from compressible volume ICV
  - QSNKCV(ICV)
* - 55
  - --
  - RVACS heat removal rate
  - QRVACS
* - 56
  - K
  - component-to-component heat transfer rate for path K
  - QCPCP(K)
* - 57
  - --
  - RVACS air flow rate
  - WAIRV2
* - 58
  - K
  - RVACS temperature for node K
  - TRVACS(K)
* - 59
  - K
  - guard vessel temperature
  - TW2RV2(K)
* - 60
  - K
  - shell inner temperature
  - TW3RV2(K)
* - 61
  - K
  - shell outer temperature
  - TW4RV2(K)
* - 62
  - K
  - outer wall temperature
  - TW5RV2(K)
* - 63
  - K
  - temperature of air between guard vessel and shell
  - TA1RV2(K)
* - 64
  - K
  - temperature of air between shell and outer wall
  - TA2RV2(K)
* - 65
  - IPMP
  - pump toque for pump IPMP
  - TQMB3(IPMP)
* - 66
  - --
  - Net reactivity
  - REANET
* - 67
  - --
  - reactivity from external function (PREA)
  - REAPRO
* - 68
  - --
  - reactivity from CRDL expansion and scram reactivity
  - REASCR
* - 69
  - --
  - Doppler reactivity feedback
  - READOP
* - 70
  - --
  - Fuel axial expansion reactivity feedback
  - READEN
* - 71
  - --
  - Radial expansion reactivity feedback
  - REAREX
* - 72
  - --
  - Coolant voiding reactivity feedback
  - REACOL
* - 73
  - --
  - Fuel relocation reactivity feedback
  - REAFUL
* - 74
  - --
  - Clad relocation reactivity feedback
  - REACLD
* - 75
  - J
  - Advanced user option for channel dependent reactivity feedback
  - REAICH(J)
* - 76
  - J
  - Normalized decay power for group J
  - POWDKH(J)
* - 77
  - IDHX
  - Heat removal rate for ADHX IDHX
  - QQ(IDHX)
* - 78
  - IDHX
  - Air flow rate for ADHX IDHX
  - WAIR(IDHX)
* - 79
  - IDHX
  - Air outlet temperature for ADHX IDHX
  - TAIROT(IDHX)
* - 80
  - IELL
  - Pressure drop in element IELL, excludes gravity head
  - DPRSEL(IELL)
* - 81
  - IPMP
  - Voltage in EM pump IPMP
  - EMVOLT(IPMP)
* - 82
  - IPMP
  - Frequency in EM pump IPMP
  - EMFREQ(IPMP)
* - 83
  - IPMP
  - Slip in EM pump IPMP
  - EMSLIP(IPMP)
* - 84
  - IPMP
  - Current in EM pump IPMP
  - EMCURR(IPMP)
* - 85
  - IPMP
  - Phase in EM pump IPMP
  - EMPHAS(IPMP)
* - 86
  - IPMP
  - Coolant Heat Generation Rate in EM pump IPMP
  - EMPCHR(IPMP)
* - 87
  - IPMP
  - Wall Heat Generation Rate in EM pump IPMP
  - EMPWHR(IPMP)
* - 88
  - IELL
  - Direct Coolant Heat in Element IELL
  - CLHEAT(IELL) 
* - 89
  - IELL
  - Direct Wall Heat in Element IELL
  - WLHEAT(IELL)                                        
""")
    with open(file) as fin:
        lines = fin.readlines()
        labels = {}
        for k,line in enumerate(lines):
            if '*' in line and "(" not in line  :
                IBNTYP = int(line.split()[-1])
                var = lines[k+3][3:].strip('\n').strip(' ')
                if "(" in var:
                    var = var.split("(")[0]
                var = var + "("
                labels[IBNTYP] = var
    os.remove(file)
    return labels


class Coolant:
    """
    Coolant is a simple class that returns the coolant properties using the SAS functional forms.

    :param name: Name of the coolant properties built into SAS (Pb, PbBi, NaK, Na, D2O).
    :param A: An array of 60 coefficients used by the SAS functional forms.
    :param Tcrit: The critical temperature of the coolant.
    :return: An object that will return coolant properties as a function of temperature.
    """
    def __init__(self, name, A = np.zeros(60), Tcrit = 1):
        self.name = name
        self.A = A
        self.Tc = Tcrit
        if name == 'Pb':
            self.Pb()
        elif name == 'PbBi':
            self.PbBi()
        elif name == 'NaK':
            self.NaK()
        elif name == 'Na':
            self.Na()
        elif name == 'D2O':
            self.D2O()


    def Lambda(self,T):
        return self.A[1] + T*(self.A[2] + T*(self.A[3] + T*self.A[4]))

    def LogPs(self,T):
        invT = 1/T
        return self.A[5]-invT*(self.A[6]-invT*self.A[7])

    def Ts(self,T):
        return self.A[8]/(self.A[9]+np.sqrt(self.A[10]+self.A[11]*self.LogPs(T)))

    def rho(self,T):
        return self.A[12] + T*(self.A[13] + T*self.A[14])

    def rhoV(self,T):
        return np.exp(self.LogPs(T))*(self.A[15]/T + self.A[16] + T*(self.A[17] + T*(self.A[18] + T*(self.A[19] + T*self.A[20]))))

    def Cp(self,T):
        dT = self.Tc-T
        invdT = 1/dT
        return invdT*(invdT*self.A[28] + self.A[29]) + self.A[30] + dT*(self.A[31] + dT*(self.A[32]))

    def CpV(self,T):
        return self.A[33] + T*(self.A[34] + T*(self.A[35] + T*(self.A[36] + T*(self.A[37] + T*(self.A[38] + T*(self.A[39]))))))

    def BetaS(self,T):
        dT = self.Tc-T
        return self.A[40] + self.A[41]/dT

    def AlphaP(self,T):
        dT = self.Tc-T
        invdT = 1/dT
        return self.A[42] + invdT*(self.A[43] + invdT*(self.A[44] + invdT*(self.A[45] + invdT*(self.A[46] + invdT*(self.A[47])))))

    def ThCon(self,T):
        return self.A[48] + T*(self.A[49] + T*(self.A[50] + T*(self.A[51])))

    def mu(self,T):
        invdT = 1/T
        return self.A[52] + invdT*(self.A[53] + invdT*(self.A[54] + invdT*(self.A[55])))

    def H(self,T):
        return self.A[56] + T*(self.A[57] + T*(self.A[58] + T*(self.A[59])))

    def Na(self):
        self.A = np.array([0.0,5.3139000E+06,-2.0296000E+03,1.0625000E+00,-3.3163000E-04,2.1690000E+01,1.1484600E+04,3.4176900E+05,6.8353800E+05,-1.1484600E+04,1.6154792E+08,-1.3670760E+06,1.0042300E+03,-2.1390000E-01,-1.1046000E-05,4.1444000E-03,-7.4461000E-06,1.3768000E-08,-1.0834000E-11,3.8903000E-15,-4.9220000E-19,1.0280100E+03,4.3248000E+03,-1.5010000E+02,2.8996000E+01,-2.9061000E+00,1.4332000E-01,-2.7431000E-03,7.3898000E+05,3.1514000E+05,1.1340000E+03,-2.2153000E-01,1.1156000E-04,2.1409000E+03,-2.2401000E+01,7.9787000E-02,-1.0618000E-04,6.7874000E-08,-2.1127000E-11,2.5834000E-15,-5.4415000E-11,4.7663000E-07,2.5156000E-06,7.9919000E-01,-6.9716000E+02,3.3140000E+05,-7.0502000E+07,5.4920000E+09,1.1045000E+02,-6.5112000E-02,1.5430000E-05,-2.4617000E-09,3.6522000E-05,1.6626000E-01,-4.5687700E+01,2.8733000E+04])
        self.Tc = 2.5033000E+03
    def NaK(self):
        self.A = np.array([0.0,5.3139000E+06,-2.0296000E+03,1.0625000E+00,-3.3163000E-04,2.1690000E+01,1.1484600E+04,3.4176900E+05,6.8353800E+05,-1.1484600E+04,1.6154792E+08,-1.3670760E+06,9.4690000E+02,-2.3930000E-01,0.0000000E+00,4.1444000E-03,-7.4461000E-06,1.3768000E-08,-1.0834000E-11,3.8903000E-15,-4.9220000E-19,1.0280100E+03,4.3248000E+03,-1.5010000E+02,2.8996000E+01,-2.9061000E+00,1.4332000E-01,-2.7431000E-03,0.0000000E+00,0.0000000E+00,1.8340000E+03,-1.1430000E+00,3.3910000E-04,2.1409000E+03,-2.2401000E+01,7.9787000E-02,-1.0618000E-04,6.7874000E-08,-2.1127000E-11,2.5834000E-15,-5.4415000E-11,4.7663000E-07,2.5156000E-06,7.9919000E-01,-6.9716000E+02,3.3140000E+05,-7.0502000E+07,5.4920000E+09,1.4180000E+01,3.2720000E-02,-2.2020000E-05,0.0000000E+00,-1.7049000E-05,1.3434000E-01,2.4114000E+01,0.0000000E+00])
        self.Tc = 2.5030000E+03
    def D2O(self):
        self.A = np.array([0.0,3.5271000E+06,-7.3309000E+03,1.5229000E+01,-1.6126000E-02,2.2979000E+01,3.5041000E+03,2.9432000E+05,5.8864000E+05,-3.5041000E+03,3.9331434E+07,-1.1772800E+06,9.1568000E+02,1.5447000E+00,-3.0790000E-03,7.0429000E-03,-6.4776000E-05,3.5815000E-07,-9.7688000E-10,1.3034000E-12,-6.6481000E-16,0.0000000E+00,0.0000000E+00,0.0000000E+00,0.0000000E+00,0.0000000E+00,0.0000000E+00,0.0000000E+00,0.0000000E+00,0.0000000E+00,5.1937000E+03,-7.1933000E+00,1.2557000E-02,3.4488000E+04,-2.8580000E+02,9.4491000E-01,-1.1398000E-03,7.8111000E-07,0.0000000E+00,0.0000000E+00,4.4329000E-12,3.9575000E-11,-4.8485000E-02,5.0206000E+01,-1.9478000E+04,3.3867000E+06,-2.2082000E+08,0.0000000E+00,-2.8088000E-01,5.0999000E-03,-8.0711000E-06,2.5964000E-09,-5.8232000E-03,7.9819161E+00,-3.5853469E+03,5.4455172E+05])
        self.Tc = 6.4450000E+02
    def Pb(self):
        self.A = np.array([0.0,8.5860000E+05,0.0000000E+00,0.0000000E+00,0.0000000E+00,2.2167800E+01,2.1193500E+04,5.3518600E+05,1.0703720E+06,-2.1193500E+04,4.9662003E+08,-2.1407440E+06,1.1441000E+04,-1.2795000E+00,0.0000000E+00,1.0000000E-02,0.0000000E+00,0.0000000E+00,0.0000000E+00,0.0000000E+00,0.0000000E+00,0.0000000E+00,0.0000000E+00,0.0000000E+00,0.0000000E+00,0.0000000E+00,0.0000000E+00,0.0000000E+00,1.0000000E+00,0.0000000E+00,2.4542200E+02,-6.6724000E-02,1.0147400E-05,0.0000000E+00,0.0000000E+00,0.0000000E+00,0.0000000E+00,0.0000000E+00,0.0000000E+00,0.0000000E+00,-2.2203800E-11,2.2403700E-07,1.7918500E-05,6.6301200E-01,-1.1353000E+03,8.4463500E+05,1.0000000E+00,1.0000000E+00,9.2000000E+00,1.1000000E-02,0.0000000E+00,0.0000000E+00,3.4805600E-04,8.6760100E-01,-2.1649500E+02,3.2591100E+05])
        self.Tc = 5.000000E+03
    def PbBi(self):
        self.A = np.array([0.0,8.5600000E+05,0.0000000E+00,0.0000000E+00,0.0000000E+00,2.3224700E+01,2.2551900E+04,7.0464400E+02,1.4092880E+03,-2.2551900E+04,5.0865365E+08,-2.8185760E+03,1.1065000E+04,-1.2930000E+00,0.0000000E+00,1.0000000E-02,0.0000000E+00,0.0000000E+00,0.0000000E+00,0.0000000E+00,0.0000000E+00,0.0000000E+00,0.0000000E+00,0.0000000E+00,0.0000000E+00,0.0000000E+00,0.0000000E+00,0.0000000E+00,1.0431900E+00,0.0000000E+00,2.4447000E+02,-6.9038500E-02,1.0753630E-05,0.0000000E+00,0.0000000E+00,0.0000000E+00,0.0000000E+00,0.0000000E+00,0.0000000E+00,0.0000000E+00,-1.8265600E-11,2.1345600E-07,1.5034900E-05,7.0562900E-01,-1.2429400E+03,9.6901400E+05,1.0000000E+00,1.0000000E+00,3.2840000E+00,1.6170000E-02,-2.3050000E-06,0.0000000E+00,3.8692200E-04,6.5425100E-01,-1.1532800E+02,1.2483800E+05])
        self.Tc = 4.8000000E+03

class ChannelGeom:
    """
    ChannelGeom is a class used by SASOutFile to store basic geometry information for a SAS channel.

    :param iCH: Channel index
    :param nZf: Number of axial nodes in the fuel.
    :param nRf: Number of radial nodes in the fuel.
    :param nZc: Number of axial nodes in the coolant. 
    :param ChannelMesh: The elevations of the nodes in the coolant mesh.
    :param FuelMesh: The elevations of the nodes in the fuel mesh.
    :return: An object that will return channel geometry information.
    """       
    def __init__(self,iCH,nZf=0,nRf=0,nZc=0):
        self.iCH = iCH
        self.nZf = 0
        self.nRf = 0
        self.nZc = 0
        self.ChannelMesh = np.zeros(nZc)
        self.FuelMesh = np.zeros([nZf,nRf])

class Channel:
    """
    Channel is a class used by SASOutFile to store basic information for a SAS channel.

    :param iCH: Channel index
    :return: An object that will return channel information.
    """         
    def __init__(self,iCH):
        self.iCH = iCH        

class SASOutFile:
    """
    SASOutFile is a class that parses and stores information from a SAS ascii output file.

    :param FileName: Name of the SAS output file.
    :param nChan: Number of channels in the SAS case. 
    :param fin: The SAS output file object. 
    :param ChanDict: A dictionary containing information about channel geometry.
    :param TimeDict: A dictionary containing data from each time step from the SAS output file. 
    :param iChan: The number of the channel that is currently being read. 
    :param iTime: The index of the time step that is currently being read. 
    :return: An object that will return axial fuel and coolant temperature distributions.
    """    
    def __init__(self,out):
        self.FileName = out
        self.nChan = -1
        self.fin = open(self.FileName,'r')
        self.ChanDict = {0 : # Top level is time, but will not be added to
                     {1 : ChannelGeom(1)} # Second level is channel
                    }
        self.TimeDict = {0 : # Top level is time step
                          {1 : Channel(1)}, # Second level is channel
                        "time" : [0],
                        "timeStep" : [0],
                    }
        self.iChan = 1 # Starting with 1 for channel index
        self.iTime = 0 # Starting with 0 for time index
        self.parse()

    def skipLines(self,skip=1):      
      for i in range(skip): self.fin.readline()

    def ChannelInit(self,i):
        self.ChanDict[0][i] = ChannelGeom(i)
        self.TimeDict[0][i] = Channel(i)

    def ReadRadFuel(self):         
        self.skipLines(2)
        temp_array = []
        line = self.fin.readline()
        while line.strip():
            temp_array.append([float(x) for x in line.split()])
            line = self.fin.readline()
        temp_array = np.array(temp_array)    
        self.ChanDict[0][self.iChan].nZf = temp_array.shape[0]
        self.TimeDict[self.iTime][self.iChan].T = temp_array[:,1:]

    def ReadCool(self):       
        self.skipLines(10)
        if not self.ChanDict[0][self.iChan].nZc:
            temp_array = []
            line = self.fin.readline()
            while "VESSEL" not in line:
                temp_array.append([float(x) for x in line.split()])
                line = self.fin.readline()
            temp_array = np.array(temp_array)
            self.ChanDict[0][self.iChan].nZc = temp_array.shape[0]
            self.TimeDict[self.iTime][self.iChan].CoolT = temp_array[:,1]
            self.TimeDict[self.iTime][self.iChan].CoolPres = temp_array[:,3]
        else:
            temp_array = []
            line = self.fin.readline()
            for i in range(self.ChanDict[0][self.iChan].nZc):
                temp_array.append([float(x) for x in line.split()])
                line = self.fin.readline()
            temp_array = np.array(temp_array)
            self.TimeDict[self.iTime][self.iChan].CoolT = temp_array[:,1]
            self.TimeDict[self.iTime][self.iChan].CoolPres = temp_array[:,3]

    def ReadTrans(self):  
        self.TimeDict[self.iTime][self.iChan].mDot = float(self.fin.readline().split()[2])
        self.skipLines(4)
        self.TimeDict[self.iTime][self.iChan].OutP = float(self.fin.readline().split()[-1])
        CoolT = []
        CoolP = []
        line = self.fin.readline()
        for i in range(self.ChanDict[0][self.iChan].nZc):
            temp_array = [float(x) for x in line.split()]
            CoolT.append(temp_array[1])
            CoolP.append(temp_array[3])
            line = self.fin.readline()
        self.TimeDict[self.iTime][self.iChan].CoolT = np.array(CoolT)
        self.TimeDict[self.iTime][self.iChan].CoolPres = np.array(CoolP)

    def ReadFuel(self, iSteady):       
        self.skipLines(3+self.ChanDict[0][self.iChan].nZf)
        if iSteady:
            self.skipLines(4)
        else:
            self.skipLines(5)
        temp_array = []
        line = self.fin.readline()
        for i in range(self.ChanDict[0][self.iChan].nZf):
            temp_array.append([float(x) for x in line.split()])
            line = self.fin.readline()
        temp_array = np.array(temp_array)
        self.TimeDict[self.iTime][self.iChan].CladInnerT = temp_array[:,1]
        self.TimeDict[self.iTime][self.iChan].CladOuterT = temp_array[:,3]
        self.TimeDict[self.iTime][self.iChan].CoolAvgT = temp_array[:,4]

    def parse(self):   
        line = self.fin.readline()
        while(line):
            line = self.fin.readline()
            if "*** SASSYS/SAS4A DATA ALLOCATION ***" in line:
                self.skipLines(2)
                self.nChan = int(self.fin.readline().split()[0])
                for i in range(2,self.nChan+1):
                    self.ChannelInit(i)
            elif re.match("^CHANNEL", line):
                self.iChan = int(line.split()[1])
            elif "*** STEADY STATE ***" in line:
                self.ReadCool()
            elif "RADIAL FUEL TEMPERATURE MESH" in line:
                self.ReadRadFuel()
            elif "*** TRANSIENT STATE RESULTS ON TIME STEP" in line:
                line = line.split("STEP")[1]
                time_step = int(line.split()[0])
                if not np.any(time_step==self.TimeDict["timeStep"]):
                    self.TimeDict["timeStep"].append(time_step)
                    self.iTime += 1
                    self.TimeDict["time"].append(float(line.split()[3]))
                    self.TimeDict[self.iTime] = self.TimeDict[self.iTime-1]
                self.ReadTrans()
            elif "RADIAL FUEL MESH" in line:
                if "IETA" in line:
                    self.ReadFuel(True)
                else:
                    self.ReadFuel(False)