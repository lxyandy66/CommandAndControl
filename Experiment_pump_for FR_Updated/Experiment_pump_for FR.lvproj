<?xml version='1.0' encoding='UTF-8'?>
<Project Type="Project" LVVersion="16008000">
	<Item Name="My Computer" Type="My Computer">
		<Property Name="server.app.propertiesEnabled" Type="Bool">true</Property>
		<Property Name="server.control.propertiesEnabled" Type="Bool">true</Property>
		<Property Name="server.tcp.enabled" Type="Bool">false</Property>
		<Property Name="server.tcp.port" Type="Int">0</Property>
		<Property Name="server.tcp.serviceName" Type="Str">My Computer/VI Server</Property>
		<Property Name="server.tcp.serviceName.default" Type="Str">My Computer/VI Server</Property>
		<Property Name="server.vi.callsEnabled" Type="Bool">true</Property>
		<Property Name="server.vi.propertiesEnabled" Type="Bool">true</Property>
		<Property Name="specify.custom.address" Type="Bool">false</Property>
		<Item Name="Experiment_pump_for_FR.vi" Type="VI" URL="../Experiment_pump_for_FR.vi"/>
		<Item Name="Dependencies" Type="Dependencies">
			<Item Name="user.lib" Type="Folder">
				<Item Name="ADO Connection Close.vi" Type="VI" URL="/&lt;userlib&gt;/labsql/LabSQL ADO functions/Connection/ADO Connection Close.vi"/>
				<Item Name="ADO Connection Create.vi" Type="VI" URL="/&lt;userlib&gt;/labsql/LabSQL ADO functions/Connection/ADO Connection Create.vi"/>
				<Item Name="ADO Connection Destroy.vi" Type="VI" URL="/&lt;userlib&gt;/labsql/LabSQL ADO functions/Connection/ADO Connection Destroy.vi"/>
				<Item Name="ADO Connection Execute.vi" Type="VI" URL="/&lt;userlib&gt;/labsql/LabSQL ADO functions/Connection/ADO Connection Execute.vi"/>
				<Item Name="ADO Connection Open.vi" Type="VI" URL="/&lt;userlib&gt;/labsql/LabSQL ADO functions/Connection/ADO Connection Open.vi"/>
				<Item Name="ADO Recordset Destroy.vi" Type="VI" URL="/&lt;userlib&gt;/labsql/LabSQL ADO functions/Recordset/ADO Recordset Destroy.vi"/>
				<Item Name="ADO Recordset GetString.vi" Type="VI" URL="/&lt;userlib&gt;/labsql/LabSQL ADO functions/Recordset/ADO Recordset GetString.vi"/>
				<Item Name="PID (SubVI).vi" Type="VI" URL="/&lt;userlib&gt;/_express/PID (SubVI).vi"/>
				<Item Name="SQL Execute.vi" Type="VI" URL="/&lt;userlib&gt;/labsql/LabSQL ADO functions/SQL Execute.vi"/>
				<Item Name="SQL Fetch Data (GetString).vi" Type="VI" URL="/&lt;userlib&gt;/labsql/LabSQL ADO functions/SQL Fetch Data (GetString).vi"/>
			</Item>
			<Item Name="vi.lib" Type="Folder">
				<Item Name="Clear Errors.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/Clear Errors.vi"/>
				<Item Name="Error Cluster From Error Code.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/Error Cluster From Error Code.vi"/>
				<Item Name="ex_CorrectErrorChain.vi" Type="VI" URL="/&lt;vilib&gt;/express/express shared/ex_CorrectErrorChain.vi"/>
				<Item Name="lvpidtkt.dll" Type="Document" URL="/&lt;vilib&gt;/addons/control/pid/lvpidtkt.dll"/>
				<Item Name="Modbus Master.lvclass" Type="LVClass" URL="/&lt;vilib&gt;/Modbus/master/Modbus Master.lvclass"/>
				<Item Name="NI_Matrix.lvlib" Type="Library" URL="/&lt;vilib&gt;/Analysis/Matrix/NI_Matrix.lvlib"/>
				<Item Name="NI_PID__prctrl compat.lvlib" Type="Library" URL="/&lt;vilib&gt;/addons/control/pid/NI_PID__prctrl compat.lvlib"/>
				<Item Name="NI_PID_pid.lvlib" Type="Library" URL="/&lt;vilib&gt;/addons/control/pid/NI_PID_pid.lvlib"/>
				<Item Name="subFile Dialog.vi" Type="VI" URL="/&lt;vilib&gt;/express/express input/FileDialogBlock.llb/subFile Dialog.vi"/>
				<Item Name="SubVIs.lvlib" Type="Library" URL="/&lt;vilib&gt;/Modbus/subvis/SubVIs.lvlib"/>
			</Item>
			<Item Name="database.lvlib" Type="Library" URL="../database/database.lvlib"/>
			<Item Name="时间查询.vi" Type="VI" URL="../时间查询.vi"/>
		</Item>
		<Item Name="Build Specifications" Type="Build"/>
	</Item>
</Project>
