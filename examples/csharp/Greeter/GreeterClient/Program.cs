using System;
using System.Linq;
using System.Net;
using System.Threading.Tasks;
using GossipMesh.Core;
using GossipMesh.LoadBalancing;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.Logging;
using Grpc.Core;
using Greeter;
using Helloworld;
using System.Diagnostics;
using System.Collections.Generic;

namespace GreeterClient
{
    class Program
    {
        public static async Task Main(string[] args)
        {
            var listenEndPoint = IPEndPointFromString(args[0]);
            var seeds = args.Skip(1).Select(IPEndPointFromString).ToArray();

            var logger = CreateLogger();

            var loadBalancer = CreateLoadBalancer();
            var gossiper = StartGossiper(listenEndPoint, seeds, new IMemberListener[] { loadBalancer }, logger);

            var stopwatch = new Stopwatch();
            while (true)
            {
                try
                {
                    Console.Write("Please enter your name: ");
                    var name = Console.ReadLine();
                    stopwatch.Restart();

                    var serviceClient = loadBalancer.GetServiceClient<GreeterServiceClient>(2);

                    var request = new HelloRequest{ Name = name};
                    var response = await serviceClient.Client.SayHelloAsync(request).ResponseAsync.ConfigureAwait(false);

                    stopwatch.Stop();
                    Console.WriteLine($"Response: {response.Message} From: {serviceClient.ServiceEndPoint} TimeTaken: {stopwatch.ElapsedMilliseconds}ms");
                }

                catch (Exception ex)
                {
                    logger.LogError(ex.Message);
                }
            }
        }

        private static ILogger CreateLogger()
        {
            var loggerFactory = new LoggerFactory();
            loggerFactory.AddProvider(new ConsoleLoggerProvider());
            return loggerFactory
                .CreateLogger<Program>();
        }

        private static RandomLoadBalancer CreateLoadBalancer()
        {
            var serviceClientFactories = new Dictionary<byte, IServiceClientFactory>
            {
                { 2, new GreeterServiceClientFactory()}
            };

            return new RandomLoadBalancer(serviceClientFactories);
        }
        private static Gossiper StartGossiper(IPEndPoint listenEndPoint, IPEndPoint[] seeds, IMemberListener[] memberListeners, ILogger logger)
        {
            var options = new GossiperOptions
            {
                MaxUdpPacketBytes = 508,
                ProtocolPeriodMilliseconds = 200,
                AckTimeoutMilliseconds = 100,
                NumberOfIndirectEndpoints = 2,
                ListenPort = (ushort)listenEndPoint.Port,
                MemberIP = listenEndPoint.Address,
                Service = (byte)3,
                ServicePort = (ushort)listenEndPoint.Port,
                SeedMembers = seeds
            };

            var gossiper = new Gossiper(options, Enumerable.Empty<IMemberEventsListener>(), memberListeners, logger);
            gossiper.Start();

            return gossiper;
        }
        private static IPEndPoint IPEndPointFromString(string ipEndPointString)
        {
            var endpoint = ipEndPointString.Split(":");
            return new IPEndPoint(IPAddress.Parse(endpoint[0]), int.Parse(endpoint[1]));
        }
    }
}