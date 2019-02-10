"use strict";

var graph = {
    "nodes": [
        { "knmp_on": false, "snmp_on": true, "ip": "192.168.2.55", "vlan": 1, "flag": 0, "netmask": "255.255.255.0", "device_name": "device_2", "mac": "00-11-22-33-44-55", "ip_on": false, "id": "s2", "gateway": "0.0.0.0" },
        { "knmp_on": true, "snmp_on": true, "ip": "192.168.2.66", "vlan": 1, "flag": 0, "netmask": "255.255.255.0", "device_name": "device_1", "mac": "11-22-33-44-55-66", "ip_on": true, "id": "s1", "gateway": "0.0.0.0" },
        { "knmp_on": true, "snmp_on": false, "ip": "192.168.2.77", "vlan": 1, "flag": 0, "netmask": "255.255.255.0", "device_name": "device_3", "mac": "22-33-44-55-66-77", "ip_on": true, "id": "s3", "gateway": "0.0.0.0" },
        { "knmp_on": false, "snmp_on": false, "ip": "192.168.2.99", "vlan": 1, "flag": 0, "netmask": "255.255.255.0", "device_name": "device_4", "mac": "44-55-66-77-88-99", "ip_on": false, "id": "s4", "gateway": "0.0.0.0" },
        { "knmp_on": true, "snmp_on": true, "ip": "192.168.2.33", "vlan": 1, "flag": 0, "netmask": "255.255.255.0", "device_name": "device_0", "mac": "55-66-77-88-99-00", "ip_on": true, "id": "s0", "gateway": "0.0.0.0" }],

    "links": [
        { "source": "s1", "target_port_disp": "port_3", "source_port_disp": "port_4", "target": "s4" },
        { "source": "s3", "target_port_disp": "port_13", "source_port_disp": "port_20", "target": "s4" },
        { "source": "s2", "target_port_disp": "port_10", "source_port_disp": "port_13", "target": "s3" },
        { "source": "s1", "target_port_disp": "port_4", "source_port_disp": "port_3", "target": "s2" },
        { "source": "s2", "target_port_disp": "port_22", "source_port_disp": "port_1", "target": "s3" },
        { "source": "s1", "target_port_disp": "port_7", "source_port_disp": "port_9", "target": "s2" },
        { "source": "s1", "target_port_disp": "port_1", "source_port_disp": "port_2", "target": "s4" },
        { "source": "s3", "target_port_disp": "port_5", "source_port_disp": "port_6", "target": "s4" }
    ]
}

initialize_topo();

var connection = new signalR.HubConnectionBuilder().withUrl("/membersHub").build();

connection.on("InitializationMessage", function (graphData, memberEvents) {
    for (var i = 0, len = memberEvents.length; i < len; i++) {
        addToList(memberEvents[i]);
    }

    load(graphData);
    // load(graph);
});

connection.on("MemberStateUpdatedMessage", function (memberEvent) {
    addToList(memberEvent);
});

function addToList(memberEvent) {
    var li = document.createElement("li");
    li.textContent = JSON.stringify(memberEvent);
    document.getElementById("membersList").appendChild(li);
}

connection.start().catch(function (err) {
    return console.error(err.toString());
});