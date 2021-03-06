﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Net;
using System.Threading.Tasks;
using GossipMesh.Core;
using GossipMesh.Seed.Hubs;
using GossipMesh.Seed.Json;
using GossipMesh.Seed.Listeners;
using GossipMesh.Seed.Stores;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.SignalR;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Logging;
using Newtonsoft.Json.Serialization;

namespace GossipMesh.Seed
{
    public class Startup
    {
        private readonly IConfiguration _configuration;
        public Startup(IConfiguration configuration)
        {
            _configuration = configuration;
        }

        public void ConfigureServices(IServiceCollection services)
        {
            services.AddSignalR()
            .AddJsonProtocol(option =>
            {
                option.PayloadSerializerSettings.Converters.Add(new IPAddressConverter());
                option.PayloadSerializerSettings.Converters.Add(new IPEndPointConverter());
                option.PayloadSerializerSettings.Converters.Add(new MemberStateConverter());
                option.PayloadSerializerSettings.Converters.Add(new DateTimeConverter());
            });

            services.AddSingleton<IMemberGraphStore, MemberGraphStore>();
            services.AddSingleton<IMemberEventsStore, MemberEventsStore>();
            services.AddSingleton<IMemberListener, MemberListener>();
            services.AddSingleton<IMemberEventsListener, MemberEventsListener>();
        }

        public void Configure(ILogger<Startup> logger, IEnumerable<IMemberListener> memberListeners, IEnumerable<IMemberEventsListener> memberEventListeners, IApplicationBuilder app, IHostingEnvironment env)
        {
            app.UseDefaultFiles();
            app.UseStaticFiles();
            app.UseSignalR(routes =>
            {
                routes.MapHub<MembersHub>("/membersHub");
            });

            var options = new GossiperOptions
            {
                MaxUdpPacketBytes = 508,
                ProtocolPeriodMilliseconds = 200,
                AckTimeoutMilliseconds = 100,
                NumberOfIndirectEndpoints = 2,
                ListenPort = ushort.Parse(_configuration["port"]),
                MemberIP = IPAddress.Parse(_configuration["ip"]),
                Service = (byte)1,
                ServicePort = (ushort)5000,
                SeedMembers = new IPEndPoint[] {},
            };

            var gossiper = new Gossiper(options, memberEventListeners, memberListeners, logger);

            gossiper.Start();
        }
    }
}
