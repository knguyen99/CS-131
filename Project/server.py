import asyncio
import sys
import time
import logging
import aiohttp
import json


 
names_to_ports = {
	"Hill": 12035,
	"Jaquez": 12036,
	"Smith": 12037,
	"Campbell": 12038,
	"Singleton": 12039
}

connections = {
	"Hill": ["Jaquez", "Smith"],
	"Jaquez": ["Hill", "Singleton"],
	"Smith": ["Hill", "Singleton", "Campbell"],
	"Campbell": ["Singleton", "Smith"],
	"Singleton": ["Jaquez", "Smith", "Campbell"]
}

KEY = "AIzaSyCDz_Xcu-bTDZCcIVa1Q5l-bh0M7fSyOUI"


def valid_int(str):
	try:
		int(str)
		return True
	except ValueError:
		return False

def loc_check(str):
	sym = []
	for i in range(len(str)):
		if str[i] == '+' or str[i] == '-':
			if i == (len(str) - 1):
				return None
			else:
				sym.append(i)

	if len(sym) != 2:
		return None
	elif str[0] != '+' and str[0] != '-':
		return None
	else:
		lat = str[0:sym[1]]
		lon = str[sym[1]:]
		return True, lat, lon

def valid_float(str):
	try:
		float(str)
		return True
	except ValueError:
		return False


#referenced TA code and https://asyncio.readthedocs.io/en/latest/tcp_echo.html
class Server:
	def __init__(self, name, num, ip='127.0.0.1', message_max_length=1e6):
		self.name = name
		self.port_no = num
		#format of clients 
			# {
			#	client: {
			#				location: [lat,lon]
			#				time:
			#				name:
			#			}
			#}
		self.clients = {}
		self.ip = ip
		self.connected = {}
		self.message_max_length = int(message_max_length)

	async def handle_echo(self, reader, writer):
		data = await reader.read()
		cur_time = time.time()
		decoded = data.decode()
		message = decoded.strip().split()
		addr = writer.get_extra_info('peername')
		if len(message) == 4 and message[0] == "IAMAT":
			loc = loc_check(message[2])
			if loc is None:
				self.invalid(decoded,addr,writer,1)
			elif not valid_float(loc[1]) or not valid_float(loc[2]) or not valid_float(message[3]):
				self.invalid(decoded,addr,writer,2)
			else:
				msg_time = float(message[3])
				if addr not in self.connected:
					self.connected[addr] = message[1]
				diff_time = cur_time - msg_time
				new_msg = "AT {} {} {} {} {}\n".format(self.name, diff_time, message[1], message[2], message[3])
				writer.write(new_msg.encode())
				if message[1] not in self.clients or float(self.clients[message[1]]["time"]) < float(message[3]):
					 self.clients[message[1]] = {"location": [loc[1],loc[2]],"name": self.name, "time": message[3] }
					 self.loop.create_task( self.flood_neighbors(new_msg))
		elif len(message) == 4 and message[0] == "WHATSAT" and valid_int(message[2]) and valid_int(message[3]):
			radius = int(message[2])
			max_results = int(message[3])
			if radius > 50 or max_results > 20 or message[1] not in self.clients:
				self.invalid(decoded,addr,writer,3)
			else:
				if addr not in self.connected:
					logging.info("connecting to client".format(message[1]))
					self.connected[addr] = message[1]
				await self.handle_whatsat(message[1], radius, max_results, cur_time, addr,writer)				
		elif len(message) == 6 and message[0] == "AT":
			#update if needed
			loc = loc_check(message[4])
			if message[3] not in self.clients or float(self.clients[message[3]]["time"]) < float(message[5]):
				self.clients[message[3]] = {"location": [loc[1],loc[2]], "name": message[1], "time": message[5] }
				self.loop.create_task( self.flood_neighbors(decoded))
		else:
			self.invalid(decoded,addr,writer,4)
	
	async def handle_whatsat(self, client,radius, max_results, cur_time, addr, writer):
		lat = self.clients[client]["location"][0]
		lon = self.clients[client]["location"][1]
		loc = lat + "," + lon
		diff_time = cur_time - float(self.clients[client]["time"])
		query = "https://maps.googleapis.com/maps/api/place/nearbysearch/json?&key={}&location={}&radius={}".format(KEY,loc,radius*1000)
		await self.query(query, diff_time, max_results, addr, writer)

	async def query(self, url, diff_time, max_results, addr, writer):
		async with aiohttp.ClientSession() as session:
			async with session.get(url) as res:
				response = await res.json()
				response['results'] = response['results'][:max_results]
				cl = self.connected[addr]
				srv_name = self.clients[cl]["name"]
				time = self.clients[cl]["time"]
				loc = "{}{}".format(self.clients[cl]["location"][0],self.clients[cl]["location"][1])
				new_msg = "AT {} {} {} {} {}\n{}\n".format(srv_name,diff_time,cl,loc, time, json.dumps(response,indent=4))
				writer.write(new_msg.encode())
				await writer.drain()

	def invalid(self, message, addr, writer, errno):
		logging.info("invalid message from {} | {} | {}".format(addr, message, errno))
		new_message = "? "+message+"\n"
		writer.write(new_message.encode())

	async def flood_neighbors(self, message):
		for neighbor in connections[self.name]:
			logging.info("connecting to neighbor {}".format(neighbor))
			try:
				reader, writer = await asyncio.open_connection(self.ip, names_to_ports[neighbor])
				logging.info("successfully connected to {}".format(neighbor))
				writer.write(message.encode())
				await writer.drain()
				writer.close()
				await writer.wait_closed()
				logging.info("connection to {} closed".format(neighbor))
			except:
				logging.info("failed to connect to {}".format(neighbor))

	def run_forever(self):
		self.loop = asyncio.get_event_loop()
		coro = asyncio.start_server(self.handle_echo, self.ip, self.port_no, loop=self.loop)
		server = self.loop.run_until_complete(coro)

		try:
			self.loop.run_forever()
		except KeyboardInterrupt:
			pass

		server.close()
		self.loop.run_until_complete(server.wait_closed())
		self.loop.close()


def main():
	#check that server name was input
	if len(sys.argv) != 2:
		print("Please enter a server name")
		sys.exit(1)

	server_name = sys.argv[1]

	#check that server name was right
	if server_name not in names_to_ports:
		print("Invalid Server Name")
		sys.exit(1)

	#grab port no
	port_no = names_to_ports[server_name]
	logging.basicConfig(filename="{}.log".format(server_name),filemode="w",level=logging.INFO)

	server = Server(server_name, port_no)
	server.run_forever()

if __name__ == '__main__':
	main()