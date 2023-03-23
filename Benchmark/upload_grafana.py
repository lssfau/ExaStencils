#! /usr/bin/env python3

import os
from influxdb import InfluxDBClient

######################
# - grafana upload - #
######################


class UploadGrafana:
    def __init__(self, json_body: dict):
        try:
            self.write_user_pw = os.environ["INFLUXDB_EXASTENCILS_PW"]
        except KeyError:
            import sys
            print('Password for the InfluxDB write_user was not set.\n',
                  'See https://docs.gitlab.com/ee/ci/variables/#secret-variables', file=sys.stderr)
            exc_info = sys.exc_info()
            raise exc_info[0].with_traceback(exc_info[1], exc_info[2])

        self.client = InfluxDBClient('i10web.informatik.uni-erlangen.de', 8086,
                                     'exastencils', self.write_user_pw, 'exastencils')

        self.client.write_points(json_body, database='exastencils', time_precision='s')
